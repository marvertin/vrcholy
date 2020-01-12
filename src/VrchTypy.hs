module VrchTypy
    ( Mou, Moustrov,
      Mnm, Bod, Hladina, Vrch(..), Sit,
     Kopec(..),
    ) where

import qualified Data.Map.Lazy as M

import Uzemi (Mou, Moustrov, Sit0)

-- Nadmořská výška v metrech
type Mnm = Int

-- Bod jako kombinace souřadnic a nadmořské výšky
type Bod = (Mou, Mnm)

-- Seznam bodů o stejné nadmořské výšce
type Hladina = ([Mou], Mnm)



-- Celá síť bodů reprezenotvaná jako mapa
type Sit = Sit0 Mnm


data Kopec = Kopec Mnm Moustrov
   deriving (Eq, Show, Read)



-- Nalezený vrchol i s atributy
data Vrch = Vrch { vrVrchol :: Kopec, -- vrchol
              vrKlicoveSedlo :: Kopec, -- klíčové sedlo vrcholu
              vrMaterskeVrcholy :: Kopec
            } -- mateřeské vrcholy, mohou být i různých výšek
      deriving (Show, Read)      
