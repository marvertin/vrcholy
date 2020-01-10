module Main where

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

dirData = "data/"
dirVysledky = "m:/Dropbox/gc/geokuk/data/"

minimalniProminence = 10


writeVysledek :: String -> [Bod] -> IO()
writeVysledek fn body = do
    let bodyVr = map (\bod@(mou, mnm) -> Vrch {vrVrchol = Kopec mnm (Moustrov [mou]), 
       vrKlicoveSedlo = Kopec 0 (Moustrov []),
       vrMaterskeVrcholy = Kopec 0 (Moustrov []) } ) body
    writeVysledekV fn bodyVr

writeVysledekV :: String -> [Vrch] -> IO()
writeVysledekV fn body = do
    let fullFn =  dirVysledky ++ fn
    putStrLn $ "Zapis "  ++ (show . length) body ++ " bodu do \"" ++ fullFn ++ "\""
    writeFile fullFn (bodyXml body)

main :: IO ()
main = do
    putStrLn $ "Potopa sveta"
    soubory <- listDirectory "data"
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory

    obsahy <- mapM B.readFile $ map (dirData ++ ) soubory
    let souradky = map fileNameToCoord soubory 

    let body = load (zip souradky obsahy)
    putStrLn $ "Pocet bodu:        " ++  (show . length) body
    -- let sit = zamapuj body
    -- putStrLn $ "Pocet bodu unique: " ++  (show . M.size) sit
    let vrcholy = potopaSveta minimalniProminence body
    putStrLn $ "Pocet vrcholu:     " ++  (show . length) vrcholy
    putStrLn $ "Pocet vrcholu:     " ++  (show . length) vrcholy
    
    writeVysledekV "vrcholy-prominence.gpx" vrcholy

po :: IO () 
po = do
    putStrLn $ "Hledani vrcholu"
    soubory <- listDirectory "data"
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory

    obsahy <- mapM B.readFile $ map (dirData ++ ) soubory
    let souradky = map fileNameToCoord soubory 

    let bodyVse = load (zip souradky obsahy)
    let body = bodyVse
    putStrLn $ "Pocet bodu:        " ++  (show . length) body

    let sit = zamapuj body
    putStrLn $ "Pocet bodu unique: " ++  (show . M.size) sit
    let kandidati = filter (jeKandidat sit) body
    putStrLn $ "Pocet kandidatu:   " ++  (show . length) kandidati
    let prominentniVrcholy = filter (jeProminentni sit) kandidati
    putStrLn $ "Pocet prominentu:  "  ++ (show . length) prominentniVrcholy
    let bezdupl = odstranDuplicity prominentniVrcholy
    putStrLn $ "Pocet ostrovu:     "  ++ (show . length) bezdupl

    
--    ostrovovani body 1000

    let vysledek = bezdupl
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml kandidati
    writeVysledek "vrcholy4.gpx" vysledek
    writeVysledek "vrcholy-duplicity.gpx" prominentniVrcholy
    writeVysledek "vrcholy-kandidati.gpx" kandidati
    -- writeVysledek "sitka.gpx" body
    putStrLn "KONEC"
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy2.gpx" $ bodyXml body2
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml (take 10000 kandidati)




