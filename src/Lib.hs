module Lib
    (
    grupuj, rozbal2, fst3, snd3, thr3
    ) where

import qualified Data.Map.Lazy as M

-- Zgrupuje seunam do mapy tak, že aplikue funkce na získání klíčů a hodnot a vrací 
-- mapu klíčů na seznamy hodnot (takže v hodnotýách budou vždy neprázdné seznamy)             
grupuj :: Ord k => (a -> k) -> (a-> v) -> [a] -> M.Map k [v]
grupuj fceKey fceValue list = 
   foldr (\x -> M.insertWith (++) (fceKey x) [fceValue x] )  M.empty list  

rozbal2 :: (a, [b]) -> [(a, b)]
rozbal2 (x, y) = zip (repeat x) y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x