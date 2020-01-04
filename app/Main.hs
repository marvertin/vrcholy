module Main where

import Lib
import Nacitac

import System.Directory
import qualified Data.ByteString as B


main :: IO ()
main = do
    soubory <- listDirectory "data"
    --obsahy <- map B.readFile soubory
    obsah <- B.readFile ("data/" ++ head soubory)
    print $ map fileNameToCoord soubory
    print (B.length  obsah)

    obsahy <- mapM B.readFile $ map ("data/" ++ ) soubory
    let souradky = map fileNameToCoord soubory 


    print $ map B.length obsahy
    
    let body = load (zip souradky obsahy)
    print $ take 50 body
    print $ length body
    

