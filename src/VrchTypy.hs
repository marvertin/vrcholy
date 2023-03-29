module VrchTypy
    ( Mou, 
      Mnm, Bod, Hladina, Vrch(..), Sit,
     -- Kopec(..),
     Misto(..)
    ) where

import qualified Data.Map.Lazy as M

import Uzemi (Mou, Sit0)

-- Nadmořská výška v metrech
type Mnm = Int

-- Bod jako kombinace souřadnic a nadmořské výšky
type Bod = (Mou, Mnm)

-- Seznam bodů o stejné nadmořské výšce
type Hladina = ([Mou], Mnm)


-- Místo je pozoce na mapě spolu s nadmořskou výškou
-- Řadíme nejprve dle nadmořské výšky, pak už je to jedno, podle které souřadnice
data Misto = Misto Mnm Mou 
  deriving (Read, Show, Eq, Ord)

-- Celá síť bodů reprezenotvaná jako mapa
type Sit = Sit0 Mnm

-- Nalezený vrchol i s atributy
data Vrch = Vrch { vrVrchol :: Misto, -- vrchol
              vrKlicoveSedlo :: Misto, -- klíčové sedlo vrcholu
              vrMaterskyVrchol :: Misto
            } -- mateřeské vrcholy, mohou být i různých výšek
      deriving (Show, Read, Ord, Eq)
