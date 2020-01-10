module Lib
    ( Mou, Mnm, Bod, Hladina, Bost(..), Vrch(..), Sit, Sit0,
     Moustrov(..), Hladka, Kopec(..),
    grupuj, jeBost, bost2vrch, jeKraj, rozbal2
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

type Hladka = (Mnm, [Moustrov])

data Kopec = Kopec Mnm Moustrov
   deriving (Eq)

-- Bod behem hledání prominence (Bod OStrovni)
data Bost = 
    Kraj -- bod je okrajovým vnitřím bodem ostrova, jenž byl redukován kvůli výkonnosti
  | Pobrezi -- právě přidaný bod, který právě vylezl na hladinu
  | Bost Hladka-- nejvyšší vrcholy ostrova, kte kterému Bost přísluší
  -- deriving (Read)

instance Show Bost where
   show Kraj = "."
   show Pobrezi = "_"
   show _ = "*"



-- newtype Kopec = Kopec 

jeKraj :: Bost -> Bool
jeKraj Kraj = True
jeKraj _ = False

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

jeBost :: Bost -> Bool
jeBost (Bost _) = True
jeBost _ = False

bost2vrch :: Bost -> Hladka
bost2vrch (Bost vrch) = vrch
bost2vrch x = error $ "nelez ziskat vrchol z " ++ (show x)

rozbal2 :: (a, [b]) -> [(a, b)]
rozbal2 (x, y) = zip (repeat x) y