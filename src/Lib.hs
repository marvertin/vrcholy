module Lib
    ( Mou, Mnm, Bod, Hladina, Bost(..), Vrch, Sit, Sit0,
    grupuj, jeBost, bost2vrch, jeKraj
    ) where

import qualified Data.Map.Lazy as M

-- Souřadnice bodu po třech vteřinách (0,0) odpovídá N0 E0
type Mou = (Int, Int)

-- Nadmořská výška v metrech
type Mnm = Int

-- Bod jako kombinace souřadnic a nadmořské výšky
type Bod = (Mou, Mnm)

-- Seznam bodů o stejné nadmořské výšce
type Hladina = ([Mou], Mnm)

type Sit0 = M.Map Mou

-- Celá síť bodů reprezenotvaná jako mapa
type Sit = Sit0 Mnm


-- Bod behem hledání prominence (Bod OStrovni)
data Bost = 
    Kraj -- bod je okrajovým vnitřím bodem ostrova, jenž byl redukován kvůli výkonnosti
  | Pobrezi -- právě přidaný bod, který právě vylezl na hladinu
  | Bost Hladina -- nejvyšší vrcholy ostrova, kte kterému Bost přísluší
  -- deriving (Read)

instance Show Bost where
   show Kraj = "."
   show Pobrezi = "_"
   show _ = "*"

jeKraj :: Bost -> Bool
jeKraj Kraj = True
jeKraj _ = False

-- Nalezený vrchol i s atributy
type Vrch = (Bod, -- vrchol
            Hladina, -- klíčové sedlo vrcholu
             [Bod]) -- mateřeské vrcholy, mohou být i různých výšek


-- Zgrupuje seunam do mapy tak, že aplikue funkce na získání klíčů a hodnot a vrací 
-- mapu klíčů na seznamy hodnot (takže v hodnotýách budou vždy neprázdné seznamy)             
grupuj :: Ord k => (a -> k) -> (a-> v) -> [a] -> M.Map k [v]
grupuj fceKey fceValue list = 
   foldr (\x -> M.insertWith (++) (fceKey x) [fceValue x] )  M.empty list  

jeBost :: Bost -> Bool
jeBost (Bost _) = True
jeBost _ = False

bost2vrch :: Bost -> Hladina
bost2vrch (Bost vrch) = vrch
bost2vrch x = error $ "nelez ziskat vrchol z " ++ (show x)