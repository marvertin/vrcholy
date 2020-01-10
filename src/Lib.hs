module Lib
    (
    grupuj, rozbal2
    ) where

import qualified Data.Map.Lazy as M

-- Zgrupuje seunam do mapy tak, že aplikue funkce na získání klíčů a hodnot a vrací 
-- mapu klíčů na seznamy hodnot (takže v hodnotýách budou vždy neprázdné seznamy)             
grupuj :: Ord k => (a -> k) -> (a-> v) -> [a] -> M.Map k [v]
grupuj fceKey fceValue list = 
   foldr (\x -> M.insertWith (++) (fceKey x) [fceValue x] )  M.empty list  

rozbal2 :: (a, [b]) -> [(a, b)]
rozbal2 (x, y) = zip (repeat x) y