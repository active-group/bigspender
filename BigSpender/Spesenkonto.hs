module BigSpender.Spesenkonto where

import BigSpender.Dinge
import BigSpender.Genehmigung
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Time.Calendar as Calendar

-- Das hier könnte in einem DST sein.

data Spesenkonto a =
    SchiebeBeleg Spesenritter Calendar.Day BelegInfo Projekt Vorgang Geld Kostenstelle (Beleg -> Spesenkonto a)
  | SpesenritterBelegStatus Spesenritter ([(Beleg, BelegStatus)] -> Spesenkonto a)
  -- für Genehmiger
  | AlleBelegStatus ([(Beleg, BelegStatus)] -> Spesenkonto a)
  | WendeGenehmigungsRegelAn Beleg Genehmigungsregel ([Genehmigung] -> Spesenkonto a)
  | PruefeBeleg Beleg BelegStatus (() -> Spesenkonto a)  -- fragwürdig
  | BeantworteRueckfrage Beleg Antwort (() -> Spesenkonto a)
  | StelleRechnung Rechnung (() -> Spesenkonto a)
  | ZahleSpesen Beleg Auszahlung (() -> Spesenkonto a)
  -- Steuern zahlen?
  | SpesenkontoFertig a

instance Functor Spesenkonto where
  fmap f (SchiebeBeleg spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) =
    SchiebeBeleg spesenritter datum belegInfo projekt vorgang geld kostenstelle (fmap f . cont)
  fmap f (SpesenritterBelegStatus spesenRitter cont) =
     SpesenritterBelegStatus spesenRitter (fmap f  . cont)
  fmap f (AlleBelegStatus cont) =
    AlleBelegStatus (fmap f . cont)
  fmap f (WendeGenehmigungsRegelAn beleg regel cont) =
    WendeGenehmigungsRegelAn beleg regel (fmap f . cont)
  fmap f (PruefeBeleg beleg belegStatus cont) =
    PruefeBeleg beleg belegStatus (fmap f . cont)
  fmap f (BeantworteRueckfrage beleg antwort cont) =
    BeantworteRueckfrage beleg antwort (fmap f . cont)
  fmap f (StelleRechnung rechnung cont) =
    StelleRechnung rechnung (fmap f . cont)
  fmap f (ZahleSpesen beleg auszahlung cont) =
    ZahleSpesen beleg auszahlung (fmap f . cont)
  fmap f (SpesenkontoFertig a) = SpesenkontoFertig (f a)

spesenkontoBind (SchiebeBeleg spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) next =
  SchiebeBeleg spesenritter datum belegInfo projekt vorgang geld kostenstelle (\beleg -> spesenkontoBind (cont beleg) next)
spesenkontoBind (SpesenritterBelegStatus spesenRitter cont) next =
   SpesenritterBelegStatus spesenRitter (\status -> spesenkontoBind (cont status) next)
spesenkontoBind (AlleBelegStatus cont) next =
   AlleBelegStatus (\status -> spesenkontoBind (cont status) next)
spesenkontoBind (WendeGenehmigungsRegelAn beleg regel cont) next =
  WendeGenehmigungsRegelAn beleg regel (\genehmigung -> spesenkontoBind (cont genehmigung) next)
spesenkontoBind (PruefeBeleg beleg belegStatus cont) next =
  PruefeBeleg beleg belegStatus (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (BeantworteRueckfrage beleg antwort cont) next =
  BeantworteRueckfrage beleg antwort (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (StelleRechnung rechnung cont) next =
  StelleRechnung rechnung (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (ZahleSpesen beleg auszahlung cont) next =
  ZahleSpesen beleg auszahlung (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (SpesenkontoFertig a) next = next a

instance Applicative Spesenkonto where
  pure = SpesenkontoFertig
  fa <*> aa = fa `spesenkontoBind` (\f -> aa `spesenkontoBind` (\a -> SpesenkontoFertig (f a)))

instance Monad Spesenkonto where
  (>>=) = spesenkontoBind

data SpesenkontoState = SpesenkontoState {
    spesenkontoStateBelegStatus :: Map Beleg BelegStatus,
    spesenkontoStateRechnungen :: [Rechnung],
    spesenkontoStateAuszahlungen :: [(Beleg, Auszahlung)]
  }

spesenkontoStateGenehmigungsKontext state antworten =
  GenehmigungsKontext {
    genehmigungsKontextStichtag = undefined,
    genehmigungsKontextBelege = Map.keys (spesenkontoStateBelegStatus state),
    genehmigungsKontextAntworten = antworten
    }

spesenkontoStateUpdateBelegStatus beleg status state =
  state { spesenkontoStateBelegStatus = Map.insert beleg status (spesenkontoStateBelegStatus state) }

nextBelegNummer state =
  maximum (map belegNummer (Map.keys (spesenkontoStateBelegStatus state))) + 1

runSpesenkonto :: Antworten -> SpesenkontoState -> Spesenkonto a -> (a, SpesenkontoState)
runSpesenkonto antworten state spesenkonto =
  run state spesenkonto
  where
   run state (SchiebeBeleg spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) =
     let beleg = Beleg { belegNummer = nextBelegNummer state,
                         belegSpesenritter = spesenritter,
                         belegDatum = undefined,
                         belegInfo = belegInfo,
                         belegProjekt = projekt,
                         belegVorgang = vorgang,
                         belegGeld = geld,
                         belegKostenstelle = kostenstelle }
     in run (spesenkontoStateUpdateBelegStatus beleg BelegStatusErfasst state)
                       (cont beleg)
   run state (SpesenritterBelegStatus spesenritter cont) =
     let belegBelongsToSpesenritter beleg = belegSpesenritter beleg == spesenritter
     in run state (cont (filter (belegBelongsToSpesenritter . fst) (Map.toList (spesenkontoStateBelegStatus state))))
   run state (AlleBelegStatus cont) =
     run state (cont (Map.toList (spesenkontoStateBelegStatus state)))
   run state (WendeGenehmigungsRegelAn beleg regel cont) =
     let status = spesenkontoStateBelegStatus state
         prozess = genehmigungsRegelProzess regel beleg (fromMaybe BelegStatusErfasst (Map.lookup beleg status)) -- FIXME
         genehmigungen = runGenehmigungsProzess (spesenkontoStateGenehmigungsKontext state antworten) prozess
--         updateBelegStatus newStatus =
--           run (spesenkontoStateUpdateBelegStatus beleg newStatus state)
--               (cont (Just genehmigung))
     in undefined      
--             in case genehmigung of
--                  GenehmigungErteilt -> updateBelegStatus BelegStatusGenehmigt
--                  GenehmigungZurueckgewiesen -> updateBelegStatus BelegStatusZurueckgewiesen
--                  GenehmigungRueckfrage rueckfrage -> updateBelegStatus  (BelegStatusRueckfrage rueckfrage)
--                  GenehmigungFrage frage -> updateBelegStatus (BelegStatusFrage frage)

   run state (PruefeBeleg beleg belegStatus cont) = -- FIXME: zu allgemein
     run (spesenkontoStateUpdateBelegStatus beleg belegStatus state)
                    (cont ())
   run state (BeantworteRueckfrage beleg antwort cont) =
     let status = spesenkontoStateBelegStatus state
     in case Map.lookup beleg status of
          Just (BelegStatusRueckfrage rueckfrage) ->
            run (spesenkontoStateUpdateBelegStatus beleg (BelegStatusRueckfrageBeantwortet rueckfrage antwort) state)
                           (cont ())
          _ -> run state (cont ()) -- fragwürdig
   run state (StelleRechnung rechnung cont) =
     let newState = state { spesenkontoStateRechnungen = rechnung : spesenkontoStateRechnungen state }
     in run newState (cont ())
   run state (ZahleSpesen beleg auszahlung cont) =
     let newState = state { spesenkontoStateAuszahlungen = (beleg, auszahlung) : spesenkontoStateAuszahlungen state }
     in run newState (cont ())
   run state (SpesenkontoFertig a) =
     (a, state)

