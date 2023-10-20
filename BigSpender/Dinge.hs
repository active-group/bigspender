module BigSpender.Dinge where

import Data.BigDecimal (BigDecimal)
import qualified Data.Time.Calendar as Calendar
import Data.List (nub)

data Spesenritter = Spesenritter {
  spesenritterName :: String
  }
  deriving (Show, Eq, Ord)

mike = Spesenritter "Mike Sperber"
eberhard = Spesenritter "Eberhard Wolff"

-- Anlass, dem mehrere Spesenbelege zuzuordnen sind,
data Vorgang = Vorgang {
  vorgangAnlass :: Anlass, -- Vorgang == Anlass?
  vorgangSpesenritter :: Spesenritter -- aus UML-Diagramm
  }
  deriving (Show, Eq, Ord)

vorgang1 = Vorgang {
  vorgangAnlass =
      Reise "Acapulco"
      (Calendar.fromGregorian 2022 Calendar.January 31)
      (Calendar.fromGregorian 2022 Calendar.December 31),
  vorgangSpesenritter = mike
  }

vorgang2 = Vorgang {
  vorgangAnlass =
      Messebesuch "OOP"
      (Calendar.fromGregorian 2022 Calendar.April 31)
      (Calendar.fromGregorian 2022 Calendar.May 2),
  vorgangSpesenritter = eberhard
  }

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
    belegInfo :: BelegInfo,
    belegVorgang :: Vorgang,
    belegDatum :: Calendar.Day, -- erfunden
    belegGeld :: Geld, -- erfunden
    belegKostenstelle :: Kostenstelle,
    belegProjekt :: Projekt -- gehört zum Vorgang?
  }
  deriving (Show, Eq, Ord)

projekt1 = Projekt "Städtetour Südamerika"

beleg1a = Beleg {
  belegNummer = 1,
  belegInfo = Flugticket,
  belegVorgang = vorgang1,
  belegDatum = Calendar.fromGregorian 2022 Calendar.January 31,
  belegGeld = Geld 1000 EUR,
  belegKostenstelle = Kostenstelle "Kund:innenbesuch",
  belegProjekt = Projekt "Städtetour Südamerika"
  }

beleg1b = Beleg {
  belegNummer = 2,
  belegInfo = Hotelrechnung,
  belegVorgang = vorgang1,
  belegDatum = Calendar.fromGregorian 2022 Calendar.December 31,
  belegGeld = Geld 300000 EUR,
  belegKostenstelle = Kostenstelle "Kund:innenbesuch",
  belegProjekt = projekt1
  }

belegSpesenritter :: Beleg -> Spesenritter
belegSpesenritter beleg = vorgangSpesenritter (belegVorgang beleg)

belegeProjekte :: [Beleg] -> [Projekt]
belegeProjekte belege = nub (map belegProjekt belege)

-- der Beleg an und für sich
data BelegInfo
  = Flugticket
  | Bahnticket
  | Taxiquittung
  | Hotelrechnung
  | Restaurantbeleg
  deriving (Show, Eq, Ord)

data Kostenstelle = Kostenstelle String
  deriving (Show, Eq, Ord)

data Kunde = Kunde String
  deriving (Show, Eq, Ord)

data Projekt = Projekt String
  deriving (Show, Eq, Ord)

data Rechnung = Rechnung {
   rechnungProjekt :: Projekt,
   rechnungLeistungen :: [Leistung],
   rechnungHonorar :: Honorar,
   rechnungSpesen :: Spesen
  }
  deriving (Show, Eq, Ord)

rechnung1 = Rechnung {
  rechnungProjekt = projekt1,
  rechnungLeistungen = [Leistung "Strandbesuch",
                        Leistung "Beratung",
                        Leistung "Barbesuch" ],
  rechnungHonorar = Honorar (Geld 300000 EUR),
  rechnungSpesen = Spesen (Geld 100000 EUR)
  }

data Geld =
    Geld BigDecimal Waehrung
  | KeinGeld
  deriving (Show, Eq)

geldWaehrung (Geld _ waehrung) = waehrung

geldBetrag KeinGeld = 0
geldBetrag (Geld betrag _) = betrag

-- | Smart Constructor
geld 0 _ = KeinGeld
geld betrag waehrung = Geld betrag waehrung

-- | Geldbetrag skalieren
-- >>> skaliereGeld 12 (Geld 10 EUR)
-- Geld 120 EUR
-- >>> skaliereGeld 1.2 (Geld 10 EUR)
-- Geld 12.0 EUR
skaliereGeld :: BigDecimal -> Geld -> Geld
skaliereGeld _ KeinGeld = KeinGeld
skaliereGeld faktor (Geld betrag waehrung) = geld (faktor * betrag) waehrung

instance Semigroup Geld where
  KeinGeld <> geld2 = geld2
  geld1 <> KeinGeld = geld1
  (Geld betrag1 waehrung1) <> (Geld betrag2 waehrung2)
    | waehrung1 == waehrung2
    = Geld (betrag1 + betrag2) waehrung1
  -- ... und bei verschiedenen Währungen?

instance Monoid Geld where
  mempty = KeinGeld

-- | Gelbeträge vergleichen
-- >>> compare (Geld 10 EUR) (Geld 12 EUR)
-- LT
-- >>> (Geld 10 EUR) < (Geld 12 EUR)
-- True
instance Ord Geld where
  compare KeinGeld KeinGeld = EQ
  compare KeinGeld geld2 =
    compare 0 (geldBetrag geld2)
  compare geld1 KeinGeld =
    compare (geldBetrag geld1) 0
  compare geld1 geld2
    | geldWaehrung geld1 == geldWaehrung geld2
    = compare (geldBetrag geld1) (geldBetrag geld2)
    
data Waehrung = EUR | USD | GBP
  deriving (Show, Eq, Ord)

data Leistung = Leistung String
  deriving (Show, Eq, Ord)

data Honorar = Honorar Geld
  deriving (Show, Eq, Ord)

newtype Spesen = Spesen Geld
  deriving (Show, Eq, Ord)

{-
Spesen haben:
- Steueranteil
- geldwerter Vorteil
- nicht erstattbare Anteile bei den Verpflegungskoste
- Erstattungsbetrag
Spesen müssen also erstattet werden, die "Regeln" dafür sind landesspezifisch.

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



