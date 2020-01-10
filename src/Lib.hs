module Lib
    ( Mou, Mnm, Bod, Hladina, Vrch(..), Sit,
     Kopec(..),
    grupuj, rozbal2
    ) where

import qualified Data.Map.Lazy as M

import Uzemi

-- Nadmořská výška v metrech
type Mnm = Int

-- Bod jako kombinace souřadnic a nadmořské výšky
type Bod = (Mou, Mnm)

-- Seznam bodů o stejné nadmořské výšce
type Hladina = ([Mou], Mnm)



-- Celá síť bodů reprezenotvaná jako mapa
type Sit = Sit0 Mnm


data Kopec = Kopec Mnm Moustrov
   deriving (Eq)



-- Nalezený vrchol i s atributy
data Vrch = Vrch { vrVrchol :: Kopec, -- vrchol
              vrKlicoveSedlo :: Kopec, -- klíčové sedlo vrcholu
              vrMaterskeVrcholy :: Kopec
            } -- mateřeské vrcholy, mohou být i různých výšek


-- Zgrupuje seunam do mapy tak, že aplikue funkce na získání klíčů a hodnot a vrací 
-- mapu klíčů na seznamy hodnot (takže v hodnotýách budou vždy neprázdné seznamy)             
grupuj :: Ord k => (a -> k) -> (a-> v) -> [a] -> M.Map k [v]
grupuj fceKey fceValue list = 
   foldr (\x -> M.insertWith (++) (fceKey x) [fceValue x] )  M.empty list  

rozbal2 :: (a, [b]) -> [(a, b)]
rozbal2 (x, y) = zip (repeat x) y