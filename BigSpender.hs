{-# LANGUAGE DerivingVia #-}
module BigSpender where

import Data.BigDecimal
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Time.Calendar as Calendar

data Spesenritter = Spesenritter {
  spesenritterName :: String
  }
  deriving (Show, Eq, Ord)

-- Anlass, dem mehrere Spesenbelege zuzuordnen sind,
data Vorgang = Vorgang {
  vorgangSpesenritter :: Spesenritter,
  vorgangAnlass :: Anlass
  }
  deriving (Show, Eq, Ord)


-- "welche meiner Belege schon erfasst wurden, welche genehmigt wurden und welche schon ausgezahlt sind" 

data Date = Date String
  deriving (Eq, Ord, Show)

data Anlass
  = Reise {
    reiseZiel :: String
    , reiseStart :: Date
    , reiseEnde :: Date
    }
  | Messebesuch {
    messeBesuchMesse :: String
    , messeBesuchStart :: Date
    , messeBesuchEnde :: Date
    }
  | Vertriebsaktion
  deriving (Show, Eq, Ord)

data Beleg = Beleg {
    belegNummer :: Int,
    belegSpesenritter :: Spesenritter,
    belegInfo :: BelegInfo,
    belegProjekt :: Projekt,
    belegVorgang :: Vorgang,
    belegGeld :: Geld,
    belegKostenstelle :: Kostenstelle
  }
  deriving (Show, Eq, Ord)

-- der Beleg an und für sich
data BelegInfo
  = Flugticket
  | Bahnticket
  | Taxiquittung
  | Hotelrechnung
  | Restaurantbeleg
  deriving (Show, Eq, Ord)

data Kostenstelle = Kostenstelle
  deriving (Show, Eq, Ord)

data Kunde = Kunde
  deriving (Show, Eq, Ord)

data Projekt = Projekt
  deriving (Show, Eq, Ord)

data Rechnung = Rechnung {
   rechnungProjekt :: Projekt,
   rechnungLeistungen :: [Leistung],
   rechnungHonorar :: Honorar,
   rechnungSpesen :: Spesen
  }
  deriving (Show, Eq, Ord)


data Geld = Geld {
    geldBetrag :: BigDecimal
  , geldWaehrung :: Waehrung
  }
  deriving (Show, Eq, Ord)

instance Num Geld where
  geld1 + geld2
    | geldWaehrung geld1 == geldWaehrung geld2
    = Geld (geldBetrag geld1 + geldBetrag geld2) (geldWaehrung geld1)

data Waehrung = EUR | USD | GBP
  deriving (Show, Eq, Ord)

data Leistung = Leistung
  deriving (Show, Eq, Ord)

data Honorar = Honorar Geld
  deriving (Show, Eq, Ord)

newtype Spesen = Spesen Geld
  deriving (Show, Eq, Ord)
  deriving Num via Geld

{-
Spesen haben:
- Steueranteil
- geldwerter Vorteil
- nicht erstattbare Anteile bei den Verpflegungskoste
- Erstattungsbetrag
Spesen müssen also erstattet werden, die "Regeln" dafür sind langesspezifisch.

-}

data Auszahlung = Auszahlung {
    auszahlungErstattung :: Geld,
    auszahlungSteueranteil :: Geld,
    auszahlungsGeldwerterVorteil :: Geld,
    auszahlungNichtErstattbar :: Geld
    }

-- wo kommt das hier hin?
data Abteilung = Abteilung {
  genehmigungsGrenze :: Geld
  }


{-
Rückfrage + Antwort

Ist "Spesen" einfach ein Haufen Belege?  Oder ein Vorgang?

Regeln: landesspezifisch
- Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als
Y Euro sind

- Wenn Spesen entstehen, die mehr als 20% vom Wochendurchschnitt des
  Projektes Z abweichen, dann löse eine Frage an den Genehmiger aus.

=> Projekt, das aber im UML-Diagramm nicht am Beleg klebt.

außerdem: Spesenkonto ... "Als Spesenritter möchte ich in meinem Spesenkonto erkennen können, welche meiner
Belege schon erfasst wurden, welche genehmigt wurden und welche schon ausgezahlt
sind"

-}

data BelegStatus =
    BelegStatusNichtErfasst -- besser Maybe?
  | BelegStatusErfasst
  | BelegStatusRueckfrage Rueckfrage
  | BelegStatusFrage Frage
  | BelegStatusRueckfrageBeantwortet Rueckfrage Antwort
  | BelegStatusGenehmigt
  | BelegStatusZurueckgewiesen
  | BelegStatusAusgezahlt Auszahlung

-- DELETEME: "offen" = "kann in den Genehmigungsprozess"
belegStatusOffen BelegStatusErfasst = True
belegStatusOffen (BelegStatusRueckfrage rueckfrage) = False
belegStatusOffen (BelegStatusFrage frage) = True
belegStatusOffen (BelegStatusRueckfrageBeantwortet rueckfrage antwort) = True
belegStatusOffen BelegStatusGenehmigt = False
belegStatusOffen (BelegStatusAusgezahlt ausgezahlt) = False

data Rueckfrage = Rueckfrage -- an den Spesenritter
data Frage = Frage -- an den Genehmiger
data Antwort = Antwort

data Genehmigung
  = GenehmigungErteilt
  | GenehmigungZurueckgewiesen
  | GenehmigungRueckfrage Rueckfrage
  | GenehmigungFrage Frage

data Genehmigungsregel =
  GenehmigungsRegel {
    genehmigungsRegelName :: String,
    genehmigungsRegelProzess :: Beleg -> BelegStatus -> GenehmigungsProzess (Maybe Genehmigung)
  }

-- Problem: verschiedene Währungen
-- Problem: Belegbeträge oder ausgezahlte Spesen?
belegeSpesen :: [Beleg] -> Geld
belegeSpesen belege = sum (map belegGeld belege)

-- "Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als Y Euro sind,
--  dann genehmige sofort und frage nicht nach."
genehmigungsregel1 beleg belegStatus =
  do let projekt = belegProjekt beleg
     belege <- holeProjektBelege projekt
     return undefined

data GenehmigungsProzess a =
    HoleProjektBelege Projekt ([Beleg] -> GenehmigungsProzess a)
  | FrageGenehmiger String (String -> GenehmigungsProzess a)
  | GenehmigungFertig a

holeProjektBelege projekt = HoleProjektBelege projekt GenehmigungFertig
frageGenehmiger frage = FrageGenehmiger frage GenehmigungFertig

instance Functor GenehmigungsProzess where
  fmap f (HoleProjektBelege projekt cont) = HoleProjektBelege projekt (fmap f . cont)
  fmap f (FrageGenehmiger text cont) = FrageGenehmiger text (fmap f . cont)
  fmap f (GenehmigungFertig a) = GenehmigungFertig (f a)

instance Applicative GenehmigungsProzess where
  pure = GenehmigungFertig
  fa <*> aa = fa `genehmigungsProzessBind` (\f ->
              aa `genehmigungsProzessBind` (\ a ->
              GenehmigungFertig (f a)))


genehmigungsProzessBind :: GenehmigungsProzess a -> (a -> GenehmigungsProzess b) -> GenehmigungsProzess b
genehmigungsProzessBind (HoleProjektBelege projekt cont) next =
  HoleProjektBelege projekt (\belege -> genehmigungsProzessBind (cont belege) next)
genehmigungsProzessBind (FrageGenehmiger text cont) next =
  FrageGenehmiger text (\antwort -> genehmigungsProzessBind (cont antwort) next)
genehmigungsProzessBind (GenehmigungFertig a) next = next a

instance Monad GenehmigungsProzess where
  (>>=) = genehmigungsProzessBind

type Antworten = Map String String

data GenehmigungsKontext = 
  GenehmigungsKontext {
    genehmigungsKontextBelege :: [Beleg],
    genehmigungsKontextAntworten :: Antworten
  }

runGenehmigungsProzess :: GenehmigungsKontext -> GenehmigungsProzess a -> a
runGenehmigungsProzess kontext (HoleProjektBelege projekt cont) =
  let isProjektBeleg beleg = belegProjekt beleg == projekt
      belege = filter isProjektBeleg (genehmigungsKontextBelege kontext)
  in runGenehmigungsProzess kontext (cont belege)
runGenehmigungsProzess kontext (GenehmigungFertig a) = a

-- Das hier könnte in einem DST sein.

data Spesenkonto a =
    SchiebeBeleg Spesenritter BelegInfo Projekt Vorgang Geld Kostenstelle (Beleg -> Spesenkonto a)
  | SpesenritterBelegStatus Spesenritter ([(Beleg, BelegStatus)] -> Spesenkonto a)
  -- für Genehmiger
  | AlleBelegStatus ([(Beleg, BelegStatus)] -> Spesenkonto a)
  | WendeGenehmigungsRegelAn Beleg Genehmigungsregel (Maybe Genehmigung -> Spesenkonto a)
  | PruefeBeleg Beleg BelegStatus (() -> Spesenkonto a)  -- fragwürdig
  | BeantworteRueckfrage Beleg Antwort (() -> Spesenkonto a)
  | StelleRechnung Rechnung (() -> Spesenkonto a)
  | ZahleSpesen Beleg Auszahlung (() -> Spesenkonto a)
  -- Steuern zahlen?
  | SpesenkontoFertig a

instance Functor Spesenkonto where
  fmap f (SchiebeBeleg spesenritter belegInfo projekt vorgang geld kostenstelle cont) =
    SchiebeBeleg spesenritter belegInfo projekt vorgang geld kostenstelle (fmap f . cont)
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

spesenkontoBind (SchiebeBeleg spesenritter belegInfo projekt vorgang geld kostenstelle cont) next =
  SchiebeBeleg spesenritter belegInfo projekt vorgang geld kostenstelle (\beleg -> spesenkontoBind (cont beleg) next)
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
   run state (SchiebeBeleg spesenritter belegInfo projekt vorgang geld kostenstelle cont) =
     let beleg = Beleg { belegNummer = nextBelegNummer state,
                         belegSpesenritter = spesenritter,
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
         prozess = genehmigungsRegelProzess regel beleg (fromMaybe BelegStatusNichtErfasst (Map.lookup beleg status))
         genehmigung = runGenehmigungsProzess (spesenkontoStateGenehmigungsKontext state antworten) prozess
     in case genehmigung of
          Nothing -> run state (cont genehmigung)
          Just genehmigung ->
             let updateBelegStatus newStatus =
                   run (spesenkontoStateUpdateBelegStatus beleg newStatus state)
                                  (cont (Just genehmigung))
             in case genehmigung of
                  GenehmigungErteilt -> updateBelegStatus BelegStatusGenehmigt
                  GenehmigungZurueckgewiesen -> updateBelegStatus BelegStatusZurueckgewiesen
                  GenehmigungRueckfrage rueckfrage -> updateBelegStatus  (BelegStatusRueckfrage rueckfrage)
                  GenehmigungFrage frage -> updateBelegStatus (BelegStatusFrage frage)

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

{-
Wo kommen die Steuervorschriften ins Spiel?

Der Regelbegriff ist mehrdeutig:

- Es gibt Regeln zur Genehmigung.

- Es gibt Regeln für die Erstattung und Versteuerung davon,
  landesspezifisch.
-}

-- FIXME: die Regeln implementieren

-- FIXME: Implementierung der Monade
