{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module BigSpender.Genehmigung where

import Data.BigDecimal (BigDecimal)
import qualified Data.BigDecimal as BigDecimal
import Data.List (sort, nub, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Time.Calendar as Calendar
import BigSpender.Dinge

newtype Rueckfrage = Rueckfrage String -- an den Spesenritter
  deriving (Eq, Ord, Show)
newtype Frage = Frage String -- an den Genehmiger
  deriving (Eq, Ord, Show)
newtype Antwort = Antwort String -- auf eine Rückfrage
  deriving (Eq, Ord, Show)

data Genehmigung = Genehmigung Beleg GenehmigungsErgebnis
 deriving (Eq, Ord, Show)

data GenehmigungsErgebnis
  = GenehmigungErteilt
  | GenehmigungZurueckgewiesen
--  | GenehmigungRueckfrage Rueckfrage
  | GenehmigungFrageGenehmiger Frage
  deriving (Eq, Ord, Show)

data Genehmigungsregel =
  GenehmigungsRegel {
    genehmigungsRegelName :: String,
    -- Beleg, BelegStatus zweifelhaft
    -- aber: vom Beleg wird das Projekt gebraucht
    genehmigungsRegelProzess :: Projekt -> GenehmigungsProzess [Genehmigung]
  }

-- Problem: verschiedene Währungen
-- Problem: Belegbeträge oder ausgezahlte Spesen?
belegeSumme :: [Beleg] -> Geld
belegeSumme belege = sum (map belegGeld belege)

data GenehmigungsProzess a =
    HoleProjektBelege Projekt ([Beleg] -> GenehmigungsProzess a)
    -- FIXME: vs. GenehmigungFrage, außerdem ist Antort für Rückfrage
  -- | FrageGenehmiger Frage (Antwort -> GenehmigungsProzess a)
  | FrageStichtag (Calendar.Day -> GenehmigungsProzess a)
  | GenehmigungFertig a

holeProjektBelege :: Projekt -> GenehmigungsProzess [Beleg]
holeProjektBelege projekt = HoleProjektBelege projekt GenehmigungFertig
-- frageGenehmiger :: Frage -> GenehmigungsProzess Antwort
-- frageGenehmiger frage = FrageGenehmiger frage GenehmigungFertig
frageStichtag :: GenehmigungsProzess Calendar.Day
frageStichtag = FrageStichtag GenehmigungFertig

instance Functor GenehmigungsProzess where
  fmap f (HoleProjektBelege projekt cont) = HoleProjektBelege projekt (fmap f . cont)
--  fmap f (FrageGenehmiger text cont) = FrageGenehmiger text (fmap f . cont)
  fmap f (FrageStichtag cont) = FrageStichtag (fmap f . cont)
  fmap f (GenehmigungFertig a) = GenehmigungFertig (f a)

instance Applicative GenehmigungsProzess where
  pure = GenehmigungFertig
  fa <*> aa = fa `genehmigungsProzessBind` (\f ->
              aa `genehmigungsProzessBind` (GenehmigungFertig . f))


genehmigungsProzessBind :: GenehmigungsProzess a -> (a -> GenehmigungsProzess b) -> GenehmigungsProzess b
genehmigungsProzessBind (HoleProjektBelege projekt cont) next =
  HoleProjektBelege projekt (\belege -> genehmigungsProzessBind (cont belege) next)
genehmigungsProzessBind (FrageStichtag cont) next =
  FrageStichtag (\stichtag -> genehmigungsProzessBind (cont stichtag) next)
-- genehmigungsProzessBind (FrageGenehmiger frage cont) next =
--  FrageGenehmiger frage (\antwort -> genehmigungsProzessBind (cont antwort) next)
genehmigungsProzessBind (GenehmigungFertig a) next = next a

instance Monad GenehmigungsProzess where
  (>>=) = genehmigungsProzessBind

type Antworten = Map Rueckfrage Antwort

data GenehmigungsKontext =
  GenehmigungsKontext {
    genehmigungsKontextStichtag :: Calendar.Day,
    genehmigungsKontextBelege :: [Beleg] -- es gibt mindestens einen
  }

runGenehmigungsProzess :: GenehmigungsKontext -> GenehmigungsProzess a -> a
runGenehmigungsProzess kontext (HoleProjektBelege projekt cont) =
  let isProjektBeleg beleg = belegProjekt beleg == projekt
      belege = filter isProjektBeleg (genehmigungsKontextBelege kontext)
  in runGenehmigungsProzess kontext (cont belege)
runGenehmigungsProzess kontext (FrageStichtag cont) =
  runGenehmigungsProzess kontext (cont (genehmigungsKontextStichtag kontext))
runGenehmigungsProzess kontext (GenehmigungFertig a) = a

montagVon :: Calendar.Day -> Calendar.Day
montagVon = Calendar.weekFirstDay Calendar.Monday

belegeDerLetztenWoche :: Projekt -> GenehmigungsProzess [Beleg]
belegeDerLetztenWoche projekt =
  do projektBelege <- holeProjektBelege projekt
     stichtag <- frageStichtag
     let letzterTag = Calendar.addDays (-1) (montagVon stichtag)
         woche = Calendar.weekAllDays Calendar.Monday letzterTag
         belege = filter (\beleg -> belegDatum beleg `elem` woche) projektBelege
     return belege

wochenDurchschnitt :: Projekt -> GenehmigungsProzess Geld
wochenDurchschnitt projekt =
 do projektBelege <- holeProjektBelege projekt
    stichtag <- frageStichtag
    let montag = montagVon stichtag
        belege = filter (\beleg -> belegDatum beleg < montag) projektBelege
        montage = nub (sort (map (montagVon . belegDatum) belege))
        belegeProWoche = map (\montag -> filter (\beleg -> montagVon (belegDatum beleg) == montag) belege) montage
        wochenSummen = map belegeSumme belegeProWoche
    return (skaliereGeld (1/fromInteger (toInteger (length wochenSummen))) (sum wochenSummen))


-- "Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als Y Euro sind, dann genehmige sofort und frage nicht nach."
genehmigungsRegel1 :: Geld -> Projekt -> GenehmigungsProzess [Genehmigung]
genehmigungsRegel1 grenze projekt =
  do belege <- belegeDerLetztenWoche projekt
     if belegeSumme belege <= grenze
     then return (map (\beleg -> Genehmigung beleg GenehmigungErteilt) belege)
     else return []

-- "Wenn Spesen entstehen, die mehr als 20% vom Wochendurchschnitt des Projektes Z abweichen, dann löse eine Frage an den Genehmiger aus."

genehmigungsregel2 :: Projekt -> GenehmigungsProzess [Genehmigung]
genehmigungsregel2 projekt =
  do durchschnitt <- wochenDurchschnitt projekt
     belege <- belegeDerLetztenWoche projekt
     if belegeSumme belege >= skaliereGeld 1.2 durchschnitt
     then return (map (\beleg -> Genehmigung beleg
                                   (GenehmigungFrageGenehmiger (Frage "Ganz schön viel Geld hier.")))
                      belege)
     else return []

-- Daraus ergibt sich: Wann steht fest, daß alle Belege einer Woche da sind?

