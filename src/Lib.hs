module Lib
    ( someFunc,
    fileNameToCoord
    ) where

import Text.Regex.Posix

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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