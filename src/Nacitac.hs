module Nacitac
    ( 
    fileNameToCoord,
    load
    ) where

import Lib

import Data.Word
import Text.Regex.Posix
import qualified Data.ByteString as B

n = 1200
souradky = [(x,y) | y <- [n,n-1..0], x <- [0..n]]

load :: [((Int, Int), B.ByteString)] -> [Bod]
load = concat . (map loadOne)

loadOne :: ((Int, Int), B.ByteString) -> [Bod]
loadOne ((rohX, rohY), bystr) = 
    let baseX = n * rohX
        baseY = n * rohY
        posunuteSouradky = map (\(x,y) -> (baseX + x, baseY + y)) souradky
    in zip posunuteSouradky (doCisel bystr)    


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
     in  ((read sirka :: Int) * znamNS, (read delka :: Int) * znamEW) 

pair :: [Word8] -> [Int]     
pair [] = []
pair (hi : lo : rest) =
    let vyska = (fromIntegral hi) * 256 + (fromIntegral lo)
    in (vyska : pair rest)


doCisel :: B.ByteString -> [Int]
doCisel = pair . B.unpack