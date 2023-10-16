{-# LANGUAGE DerivingVia #-}
module BigSpender.Dinge where

import Data.BigDecimal (BigDecimal)
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

data Anlass
  = Reise {
    reiseZiel :: String,
    reiseStart :: Calendar.Day,
    reiseEnde :: Calendar.Day
    }
  | Messebesuch {
    messeBesuchMesse :: String,
    messeBesuchStart :: Calendar.Day,
    messeBesuchEnde :: Calendar.Day
    }
  | Vertriebsaktion
  deriving (Show, Eq, Ord)

data Beleg = Beleg {
    belegNummer :: Int,
    belegSpesenritter :: Spesenritter,
    belegDatum :: Calendar.Day,
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
  deriving (Show, Eq)

skaliereGeld :: BigDecimal -> Geld -> Geld
-- >>> skaliereGeld 12 (Geld 10 EUR)
-- Geld {geldBetrag = 120, geldWaehrung = EUR}
skaliereGeld faktor geld = geld { geldBetrag = faktor * geldBetrag geld }

-- >>> (Geld 10 EUR) + (Geld 12 EUR)
-- Geld {geldBetrag = 22, geldWaehrung = EUR}
instance Num Geld where
  geld1 + geld2
    | geldWaehrung geld1 == geldWaehrung geld2
    = Geld (geldBetrag geld1 + geldBetrag geld2) (geldWaehrung geld1)

-- >>> compare (Geld 10 EUR) (Geld 12 EUR)
-- LT
-- >>> (Geld 10 EUR) < (Geld 12 EUR)
-- True
instance Ord Geld where
  compare geld1 geld2
    | geldWaehrung geld1 == geldWaehrung geld2
    = compare (geldBetrag geld1) (geldBetrag geld2)
    
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



