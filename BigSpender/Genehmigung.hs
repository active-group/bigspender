module BigSpender.Genehmigung where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import BigSpender.Dinge

data Rueckfrage = Rueckfrage -- an den Spesenritter
data Frage = Frage -- an den Genehmiger
data Antwort = Antwort

data Genehmigung
  = GenehmigungErteilt
  | GenehmigungZurueckgewiesen
  | GenehmigungRueckfrage Rueckfrage
  | GenehmigungFrage Frage
  
data BelegStatus
  = BelegStatusErfasst
  | BelegStatusFrage Frage
  | BelegStatusRueckfrage Rueckfrage
  | BelegStatusRueckfrageBeantwortet Rueckfrage Antwort

data Genehmigungsregel =
  GenehmigungsRegel {
    genehmigungsRegelName :: String,
    genehmigungsRegelProzess :: Beleg -> BelegStatus -> GenehmigungsProzess (Maybe Genehmigung)
  }

-- Problem: verschiedene Währungen
-- Problem: Belegbeträge oder ausgezahlte Spesen?
belegeSpesen :: [Beleg] -> Geld
belegeSpesen belege = sum (map belegGeld belege)

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

-- "Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als Y Euro sind, dann genehmige sofort und frage nicht nach."
-- genehmigungsRegel1 grenze beleg

-- "Wenn Spesen für Projekt X anfallen, die am Ende der Woche insgesamt kleiner als Y Euro sind,
--  dann genehmige sofort und frage nicht nach."
genehmigungsregel2 beleg belegStatus =
  do let projekt = belegProjekt beleg
     belege <- holeProjektBelege projekt
     return undefined

