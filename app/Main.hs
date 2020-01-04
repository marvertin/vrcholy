module Main where

import Lib
import Nacitac
import Zapisovac
import Hledac

import System.Directory
import qualified Data.ByteString as B

jeCtverec :: Bod -> Bool
jeCtverec ((x, y),_) = x > 20100 && y > 59380 && x < 20200 &&  y < 59480

main :: IO ()
main = do
    putStrLn $ "Hledani vrcholu"
    soubory <- listDirectory "data"
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:    " ++  (show . length) soubory

    obsahy <- mapM B.readFile $ map ("data/" ++ ) soubory
    let souradky = map fileNameToCoord soubory 

    let bodyVse = load (zip souradky obsahy)
    let body = bodyVse
    putStrLn $ "Pocet bodu:       " ++  (show . length) body
    let body2 = filter jeCtverec bodyVse
    let podivne = filter (\(_,v) -> v > 1600) body

    let sit = zamapuj body
    let kandidati = filter (jeKandidat sit) body
    putStrLn $ "Pocet kandidatu:  " ++  (show . length) kandidati
    let prominentniVrcholy = filter (jeProminentni sit) kandidati
    putStrLn $ "Pocet prominentu: "  ++ (show . length) prominentniVrcholy

    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml kandidati
    writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml prominentniVrcholy
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy2.gpx" $ bodyXml body2
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml (take 10000 kandidati)




