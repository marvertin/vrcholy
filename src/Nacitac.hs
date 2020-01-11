module Nacitac
    ( 
    fileNameToCoord,
    loadSrtms,
    loadSrtm
    ) where

import Lib
import VrchTypy(Bod)
import Uzemi

import Data.Int
import Data.Word
import Data.Tuple
import Text.Regex.Posix
import qualified Data.ByteString as B 

n = 1200
souradky = [ Mou x y | y <- [n,n-1..0], x <- [0..n]]

loadSrtms :: [((Int, Int), B.ByteString)] -> [Bod]
loadSrtms = concat . (map loadSrtm)

loadSrtm :: ((Int, Int), B.ByteString) -> [Bod]
loadSrtm ((rohX, rohY), bystr) = 
    let baseX = n * rohX
        baseY = n * rohY
        bodyNeposunute = orezDuplicitniKraje $ zip souradky (doCisel bystr)    
        posunuteSouradky = map (addMou baseX baseY) souradky
    in map (swap . fmap (addMou baseX baseY) . swap) bodyNeposunute

fmap1 :: (a -> c) -> (a, b) -> (c, b)
fmap1 f (x,y) = (f x, y)
-- loadOne :: B.ByteString -> [Bod]
fileNameToCoord :: String -> (Int, Int)
fileNameToCoord fileName =
    let (_,_,_,([ns, sirka, ew, delka])) = fileName =~ "(N|S)([0-9]+)(E|W)([0-9]+).hgt" ::  (String,String,String,[String])
        znamNS = case ns of
            "N" -> 1
            "S" -> -1
        znamEW = case ew of
            "E" -> 1
            "W" -> -1
     in  ((read delka :: Int) * znamNS, (read sirka :: Int) * znamEW) 

pair :: [Word8] -> [Int]     
pair [] = []
pair (hi : lo : rest) =
    let vyskaInt16 = (fromIntegral hi) * 256 + (fromIntegral lo) :: Int16
        vyska = fromIntegral vyskaInt16 :: Int
    in (vyska : pair rest)


doCisel :: B.ByteString -> [Int]
doCisel = pair . B.unpack

orezDuplicitniKraje :: [Bod] -> [Bod]
orezDuplicitniKraje = filter (\ (Mou x y, _) -> x < 1200 && y < 1200)
