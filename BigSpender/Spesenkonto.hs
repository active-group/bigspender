{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module BigSpender.Spesenkonto where

import BigSpender.Dinge
import BigSpender.Genehmigung
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Time.Calendar as Calendar

-- Das hier könnte in einem DST sein.

data Spesenkonto a =
  -- "Als Beleg-Scanner möchte ich Spesenbelege einscannen, 
  --  im System ablegen und einer Kostenstelle zuordnen."
    LegeBelegAb Spesenritter Calendar.Day BelegInfo Projekt Vorgang Geld Kostenstelle (Beleg -> Spesenkonto a)
  -- "Als Spesenritter möchte ich in meinem Spesenkonto erkennen 
  --  können, welche meiner Belege schon erfasst wurden,
  --  welche genehmigt wurden und welche schon ausgezahlt sind."
  | SpesenritterBelegStatus Spesenritter ([(Beleg, BelegStatus)] -> Spesenkonto a)
  -- "Als Genehmiger möchte ich per E-Mail über einen zu 
  --  genehmigenden Vorgang informiert werden, aber nur wenn
  --  dessen Spesenbetrag ein eingestelltes Limit überschreitet."
  | GenehmigerGenehmigungsStatus ([(Beleg, GenehmigungsStatus)] -> Spesenkonto a)
  -- ...
  | WendeGenehmigungsRegelAn Genehmigungsregel Beleg (() -> Spesenkonto a)
  -- "Als Genehmiger möchte ich eine Rückfrage zu einem Vorgang an den Spesenritter stellen können."
  | StelleRueckfrage Beleg Frage (() -> Spesenkonto a) 
  | Genehmige Beleg (() -> Spesenkonto a)
  | LehneAb Beleg (() -> Spesenkonto a)

  | BeantworteRueckfrage Beleg Antwort (() -> Spesenkonto a)

  | StelleRechnung Rechnung (() -> Spesenkonto a)
  | ZahleSpesen Beleg Auszahlung (() -> Spesenkonto a)
  -- Steuern zahlen?
  | SpesenkontoFertig a


-- was das System speichert
data BelegStatus =
    BelegStatusGenehmigt
  | BelegStatusAbgelehnt
  | BelegStatusInBearbeitung
  | BelegStatusRueckfrage Frage
  | BelegStatusRueckfrageBeantwortet Frage Antwort
  | BelegStatusGenehmigerFrage [Frage] -- dürfen Spesenritter sehen?
  | BelegStatusAusgezahlt
  deriving (Eq, Ord, Show)

-- was für den Genehmiger relevant ist
data GenehmigungsStatus =
    GenehmigungsStatusGenehmigerFrage [Frage]
  | GenehmigungsStatusRueckfrage Frage
  | GenehmigungsStatusRueckfrageBeantwortet Frage Antwort

belegStatusToGenehmigungsStatus
  :: BelegStatus -> Maybe GenehmigungsStatus
belegStatusToGenehmigungsStatus (BelegStatusGenehmigerFrage fragen) =
  Just (GenehmigungsStatusGenehmigerFrage fragen)
belegStatusToGenehmigungsStatus (BelegStatusRueckfrage frage) =
  Just (GenehmigungsStatusRueckfrage frage)
belegStatusToGenehmigungsStatus (BelegStatusRueckfrageBeantwortet frage antwort) =
  Just (GenehmigungsStatusRueckfrageBeantwortet frage antwort)
belegStatusToGenehmigungsStatus _ = Nothing

instance Functor Spesenkonto where
  fmap :: (a -> b) -> Spesenkonto a -> Spesenkonto b
  fmap f (LegeBelegAb spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) =
    LegeBelegAb spesenritter datum belegInfo projekt vorgang geld kostenstelle (fmap f . cont)
  fmap f (SpesenritterBelegStatus spesenRitter cont) =
     SpesenritterBelegStatus spesenRitter (fmap f  . cont)
  fmap f (GenehmigerGenehmigungsStatus cont) =
    GenehmigerGenehmigungsStatus (fmap f . cont)
  fmap f (WendeGenehmigungsRegelAn regel beleg cont) =
    WendeGenehmigungsRegelAn regel beleg (fmap f . cont)
  fmap f (StelleRueckfrage beleg frage cont) =
    StelleRueckfrage beleg frage (fmap f . cont)
  fmap f (BeantworteRueckfrage beleg antwort cont) =
    BeantworteRueckfrage beleg antwort (fmap f . cont)
  fmap f (Genehmige beleg cont) =
    Genehmige beleg (fmap f . cont)
  fmap f (LehneAb beleg cont) =
    LehneAb beleg (fmap f . cont)
  fmap f (StelleRechnung rechnung cont) =
    StelleRechnung rechnung (fmap f . cont)
  fmap f (ZahleSpesen beleg auszahlung cont) =
    ZahleSpesen beleg auszahlung (fmap f . cont)
  fmap f (SpesenkontoFertig a) = SpesenkontoFertig (f a)

spesenkontoBind (LegeBelegAb spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) next =
  LegeBelegAb spesenritter datum belegInfo projekt vorgang geld kostenstelle (\beleg -> spesenkontoBind (cont beleg) next)
spesenkontoBind (SpesenritterBelegStatus spesenRitter cont) next =
   SpesenritterBelegStatus spesenRitter (\status -> spesenkontoBind (cont status) next)
spesenkontoBind (GenehmigerGenehmigungsStatus cont) next =
   GenehmigerGenehmigungsStatus (\status -> spesenkontoBind (cont status) next)
spesenkontoBind (WendeGenehmigungsRegelAn regel beleg cont) next =
  WendeGenehmigungsRegelAn regel beleg (\genehmigung -> spesenkontoBind (cont genehmigung) next)
spesenkontoBind (StelleRueckfrage beleg frage cont) next =
  StelleRueckfrage beleg frage (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (Genehmige beleg cont) next =
  Genehmige beleg (\() -> spesenkontoBind (cont ()) next)
spesenkontoBind (LehneAb beleg cont) next =
  LehneAb beleg (\() -> spesenkontoBind (cont ()) next)
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
    -- FIXME: Historie pro Beleg
    spesenkontoStateRechnungen :: [Rechnung],
    spesenkontoStateAuszahlungen :: [(Beleg, Auszahlung)]
  }

spesenkontoStateGenehmigungsKontext state =
  GenehmigungsKontext {
    genehmigungsKontextStichtag = undefined, -- FIXME
    genehmigungsKontextBelege = Map.keys (spesenkontoStateBelegStatus state)
    }

spesenkontoStateUpdateBelegStatus
  :: Beleg -> BelegStatus -> SpesenkontoState -> SpesenkontoState
spesenkontoStateUpdateBelegStatus beleg status state =
  state { spesenkontoStateBelegStatus = Map.insert beleg status (spesenkontoStateBelegStatus state) }

nextBelegNummer :: SpesenkontoState -> Int
nextBelegNummer state =
  maximum (map belegNummer (Map.keys (spesenkontoStateBelegStatus state))) + 1

runSpesenkonto :: Calendar.Day -> Antworten -> SpesenkontoState -> Spesenkonto a -> (a, SpesenkontoState)
runSpesenkonto stichtag antworten state spesenkonto =
  run state spesenkonto
  where
   run state (LegeBelegAb spesenritter datum belegInfo projekt vorgang geld kostenstelle cont) =
     let beleg = Beleg { belegNummer = nextBelegNummer state,
                         belegDatum = datum,
                         belegInfo = belegInfo,
                         belegProjekt = projekt,
                         belegVorgang = vorgang,
                         belegGeld = geld,
                         belegKostenstelle = kostenstelle }
     in run (spesenkontoStateUpdateBelegStatus beleg BelegStatusInBearbeitung state)
                       (cont beleg)

   run state (SpesenritterBelegStatus spesenritter cont) =
     let belegBelongsToSpesenritter beleg = belegSpesenritter beleg == spesenritter
     in run state (cont (filter (belegBelongsToSpesenritter . fst) (Map.toList (spesenkontoStateBelegStatus state))))

   run state (GenehmigerGenehmigungsStatus cont) =
     let pairs = mapMaybe (\(beleg, belegStatus) ->
                           case belegStatusToGenehmigungsStatus belegStatus of
                             Nothing -> Nothing
                             Just genehmigungsStatus -> Just (beleg, genehmigungsStatus))
                           (Map.toList (spesenkontoStateBelegStatus state))
     in run state (cont pairs)

   run state (WendeGenehmigungsRegelAn regel beleg cont) =
     case wendeGenehmigungsRegelAn stichtag (map fst (Map.toList (spesenkontoStateBelegStatus state))) regel beleg of
       Just ergebnis ->
         let belegStatus = case ergebnis of
                             GenehmigungErteilt -> BelegStatusGenehmigt
                             GenehmigungZurueckgewiesen -> BelegStatusAbgelehnt
                             -- was ist mit schon vorhandenen Fragen?  Rueckfragen?
                             GenehmigungFrageGenehmiger fragen -> BelegStatusGenehmigerFrage fragen
         in run (spesenkontoStateUpdateBelegStatus beleg belegStatus state) (cont ())
       Nothing -> run state (cont ())

   run state (StelleRueckfrage beleg frage cont) =
     run (spesenkontoStateUpdateBelegStatus beleg (BelegStatusRueckfrage frage) state)
         (cont ())
   run state (Genehmige beleg cont) =
     run (spesenkontoStateUpdateBelegStatus beleg BelegStatusGenehmigt state)
         (cont ())
   run state (LehneAb beleg cont) =
     run (spesenkontoStateUpdateBelegStatus beleg BelegStatusAbgelehnt state)
         (cont ())


   run state (BeantworteRueckfrage beleg antwort cont) =
     case Map.lookup beleg (spesenkontoStateBelegStatus state) of
       Just (BelegStatusRueckfrage frage) ->
         run (spesenkontoStateUpdateBelegStatus beleg (BelegStatusRueckfrageBeantwortet frage antwort) state)
             (cont ())
       _ -> run state (cont ())

   run state (StelleRechnung rechnung cont) =
     let newState = state { spesenkontoStateRechnungen = rechnung : spesenkontoStateRechnungen state }
     in run newState (cont ())
   run state (ZahleSpesen beleg auszahlung cont) =
     let newState = state { spesenkontoStateAuszahlungen = (beleg, auszahlung) : spesenkontoStateAuszahlungen state }
     in run newState (cont ())
   run state (SpesenkontoFertig a) =
     (a, state)


