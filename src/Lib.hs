module Lib
    (
    grupuj, rozbal2, fst3, snd3, thr3, kazdyPrvekAZbytky, rostouci
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

-- Ze seznamu vytáhne každá zprvků jednou jako první prvek
-- vnořených seznamů, ostatní následují v původním pořadí
kazdyPrvekAZbytky :: [a] -> [[a]]
kazdyPrvekAZbytky sez = kaz [] sez
  where
    kaz :: [a] -> [a] -> [[a]]
    kaz _ [] = []
    kaz s1 (x : s2) =  (x : s1 ++ s2) :  kaz (s1 ++ [x])  s2 

-- Zajistí, že řada čísel bude ostře rostoucí  1,2,2,13,40,40,55 -> 1,2,3,13,40,41,55
rostouci :: (Enum a, Ord a) => [a] -> [a]
rostouci [] = []
rostouci [a] = [a]
rostouci (a : b : rest) = (a : rostouci (max b (succ a) : rest))