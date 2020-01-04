module Main where

import Lib
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

    print $ map B.length obsahy
    

