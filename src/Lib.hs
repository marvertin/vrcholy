module Lib
    (
    grupuj, rozbal2, fst3, snd3, thr3, kazdyPrvekAZbytky, rostouci, base34,
    readLinesFromDir
    ) where

import qualified Data.Map.Lazy as M
import Data.Char (chr, isSpace)
import System.FilePath.Posix
import System.Directory
import Control.Monad

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

-- vrátí neprázdné řádky ze všech souborů v adresáři
readLinesFromDir :: FilePath -> IO [String]
readLinesFromDir dirname = do
  files <- listDirectory dirname
  fmap (filter (not . all isSpace) . concat) . forM files $ \filename -> do
    fmap lines . readFile $ dirname </> filename

base34 ::  Integer -> String
base34 0 = ""
base34 n | n < 0 = error $ "zaporne cislo do base34: " ++ show n
base34 n = let
    zbytek = n `mod` 34
    podil =  n `div` 34
   in base34(podil) ++ [cnvt zbytek]


cnvt 0 = '0'
cnvt 1 = '1'
cnvt 2 = '2'
cnvt 3 = '3'
cnvt 4 = '4'
cnvt 5 = '5'
cnvt 6 = '6'
cnvt 7 = '7'
cnvt 8 = '8'
cnvt 9 = '9'
cnvt 10 = 'A'
cnvt 11 = 'B'
cnvt 12 = 'C'
cnvt 13 = 'D'
cnvt 14 = 'E'
cnvt 15 = 'F'
cnvt 16 = 'G'
cnvt 17 = 'H'
cnvt 18 = 'J'
cnvt 19 = 'K'
cnvt 20 = 'L'
cnvt 21 = 'M'
cnvt 22 = 'N'
cnvt 23 = 'P'
cnvt 24 = 'Q'
cnvt 25 = 'R'
cnvt 26 = 'S'
cnvt 27 = 'T'
cnvt 28 = 'U'
cnvt 29 = 'V'
cnvt 30 = 'W'
cnvt 31 = 'X'
cnvt 32 = 'Y'
cnvt 33 = 'Z'
