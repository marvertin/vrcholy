module Ladeni where

import Lib
import Nacitac
import Zapisovac
import Hledac
import Potopa
import Uzemi
import VrchTypy

import System.Directory
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import  Control.Arrow


jeCtverec :: Bod -> Bool
jeCtverec (Mou x y,_) = x > 20100 && y > 59380 && x < 20200 &&  y < 59480

ostrovovani :: [Bod] -> Int -> IO()
ostrovovani body mez = do
    let ostrN = rozdelNaOstrovy (zamapuj (filter ((> mez) . snd)  body) );
    putStrLn $ "Pocet ostrovu > "  ++ show mez ++ "     " ++ (show . length) ostrN

gener100  = [((x,y), x+y) | x <- [1..100], y <- [1..100]]    
gener100a = [((x,y), min (x+y) 40) | x <- [1..100], y <- [1..100]]    
gener100b = [((x,y), y) | x <- [1..100], y <- [1..100]]    
gener10x= [((x,y), 45) | x <- [1..10], y <- [1..10]]    
gener4 = [((x,y), y) | x <- [1..4], y <- [1..4]]    
gener6 = [((x,y), x) | x <- [1..6], y <- [1..6]]    
gener2 = [((x,y), 444) | x <- [1..2], y <- [1..2]]    
gener1 = [((5,5),42)]    
gener0 = []    

gener100x  = [((x,y), x - (dolu x y) )  | x <- [1..100], y <- [1..100]]  

namou :: [((Int, Int), Int)] -> [Bod]
namou = map (first (uncurry Mou))

dolu :: Int -> Int -> Int
dolu 50 51 = 10
dolu 50 52 = 10
dolu 50 53 = 10
dolu 51 51 = 16
dolu 51 52 = 16
dolu 51 53 = 16
dolu 52 51 = 10
dolu 52 52 = 10
dolu 52 53 = 10
--dolu 30 50 = 10
dolu _ _ = 0


pp = do
    let pust = gener100x
    putStrLn $ "generovane: " ++ (show.length) pust
--    print $ rozhladinuj pust
    let vrcholy = potopaSvetaZBodu 0 (namou pust)
    putStrLn $ "Pocet vrcholu:     " ++  (show . length) vrcholy
    putStrLn $ "Pocet vrcholu:     " ++  (show . length) vrcholy

