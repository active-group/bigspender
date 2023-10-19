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

data GenehmigungsErgebnis
  = GenehmigungErteilt
  | GenehmigungZurueckgewiesen
  | GenehmigungFrageGenehmiger [Frage]
  deriving (Eq, Ord, Show)

instance Semigroup GenehmigungsErgebnis where
  GenehmigungErteilt <> _ = GenehmigungErteilt
  _ <> GenehmigungErteilt = GenehmigungErteilt
  GenehmigungZurueckgewiesen <> _ = GenehmigungZurueckgewiesen
  _ <> GenehmigungZurueckgewiesen = GenehmigungZurueckgewiesen
  GenehmigungFrageGenehmiger fragen1 <> GenehmigungFrageGenehmiger fragen2 = GenehmigungFrageGenehmiger (fragen1 ++ fragen2)
  

data Genehmigungsregel =
  GenehmigungsRegel {
    genehmigungsregelName :: String,
    genehmigungsregelProzess :: Beleg -> GenehmigungsProzess (Maybe GenehmigungsErgebnis)
  }

-- Problem: verschiedene Währungen
-- Problem: Belegbeträge oder ausgezahlte Spesen?

-- | Summe der Beträge einer Menge von Belegen
-- >>> belegeSumme [beleg1a, beleg1b]
-- Geld 301000 EUR
belegeSumme :: [Beleg] -> Geld
belegeSumme belege = mconcat (map belegGeld belege)

data GenehmigungsProzess a =
    HoleProjektBelege Projekt ([Beleg] -> GenehmigungsProzess a)
  | FrageStichtag (Calendar.Day -> GenehmigungsProzess a)
  | GenehmigungFertig a

holeProjektBelege :: Projekt -> GenehmigungsProzess [Beleg]
holeProjektBelege projekt = HoleProjektBelege projekt GenehmigungFertig
frageStichtag :: GenehmigungsProzess Calendar.Day
frageStichtag = FrageStichtag GenehmigungFertig

instance Functor GenehmigungsProzess where
  fmap f (HoleProjektBelege projekt cont) = HoleProjektBelege projekt (fmap f . cont)
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

-- | Den Montag vor einem gegebenen Tag ermitteln
-- >>> montagVon (Calendar.fromGregorian 2022 Calendar.December 31)
-- 2022-12-26
montagVon :: Calendar.Day -> Calendar.Day
montagVon = Calendar.weekFirstDay Calendar.Monday

wendeGenehmigungsRegelAn :: Calendar.Day -> [Beleg] -> Genehmigungsregel -> Beleg -> Maybe GenehmigungsErgebnis
wendeGenehmigungsRegelAn stichtag belege regel beleg =
  runGenehmigungsProzess (GenehmigungsKontext stichtag belege) (genehmigungsregelProzess regel beleg)

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
    return (skaliereGeld (1/fromInteger (toInteger (length wochenSummen))) (mconcat wochenSummen))

-- | Genehmigungsregel #1 aus der Aufgabenstellung
-- >>> wendeGenehmigungsRegelAn (Calendar.fromGregorian 2023 Calendar.January 4) [beleg1a, beleg1b] (genehmigungsregel1 (Geld 10 EUR)) beleg1a
-- Nothing
-- >>> wendeGenehmigungsRegelAn (Calendar.fromGregorian 2023 Calendar.January 4) [beleg1a, beleg1b] (genehmigungsregel1 (Geld 300000 EUR)) beleg1a
-- Just GenehmigungErteilt
genehmigungsregel1 :: Geld -> Genehmigungsregel
genehmigungsregel1 grenze =
  let prozess beleg =
        do let projekt = belegProjekt beleg
           belege <- belegeDerLetztenWoche projekt
           if belegeSumme belege <= grenze
           then return (Just GenehmigungErteilt)
           else return Nothing
  in GenehmigungsRegel {
       genehmigungsregelName = "Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als Y Euro sind, dann genehmige sofort und frage nicht nach.",
       genehmigungsregelProzess = prozess
     }


-- | Genehmigungsregel #2 aus der Aufgabenstellung
-- >>> wendeGenehmigungsRegelAn (Calendar.fromGregorian 2023 Calendar.January 4) [beleg1a, beleg1b] genehmigungsregel2 beleg1a
-- Just (GenehmigungFrageGenehmiger [Frage "Ganz sch\246n viel Geld hier."])
genehmigungsregel2 :: Genehmigungsregel
genehmigungsregel2 =
  let prozess beleg =
        do let projekt = belegProjekt beleg
           durchschnitt <- wochenDurchschnitt projekt
           belege <- belegeDerLetztenWoche projekt
           if belegeSumme belege >= skaliereGeld 1.2 durchschnitt
           then return (Just (GenehmigungFrageGenehmiger [Frage "Ganz schön viel Geld hier."]))
           else return Nothing
  in GenehmigungsRegel {
       genehmigungsregelName = "Wenn Spesen entstehen, die mehr als 20% vom Wochendurchschnitt des Projektes Z abweichen, dann löse eine Frage an den Genehmiger aus.",
       genehmigungsregelProzess = prozess
     }

-- Daraus ergibt sich: Wann steht fest, daß alle Belege einer Woche da sind?

