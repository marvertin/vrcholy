module Main where

import Lib
import Nacitac
import Zapisovac
import Hledac

import System.Directory
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M

dirData = "data/"
dirVysledky = "m:/Dropbox/gc/geokuk/data/"

writeVysledek :: String -> [Bod] -> IO()
writeVysledek fn body = do
    let fullFn =  dirVysledky ++ fn
    putStrLn $ "Zapis "  ++ (show . length) body ++ " bodu do \"" ++ fullFn ++ "\""
    writeFile fullFn $ bodyXml body


jeCtverec :: Bod -> Bool
jeCtverec ((x, y),_) = x > 20100 && y > 59380 && x < 20200 &&  y < 59480

ostrovovani :: [Bod] -> Int -> IO()
ostrovovani body mez = do
    let ostrN = rozdelNaOstrovy (zamapuj (filter ((> mez) . snd)  body) );
    putStrLn $ "Pocet ostrovu > "  ++ show mez ++ "     " ++ (show . length) ostrN

main :: IO ()
main = do
    putStrLn $ "Hledani vrcholu"
    soubory <- listDirectory "data"
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory

    obsahy <- mapM B.readFile $ map (dirData ++ ) soubory
    let souradky = map fileNameToCoord soubory 

    let bodyVse = load (zip souradky obsahy)
    let body = bodyVse
    putStrLn $ "Pocet bodu:        " ++  (show . length) body
    let body2 = filter jeCtverec bodyVse
    let podivne = filter (\(_,v) -> v > 1600) body

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
    putStrLn $ "Zapis vysledku:     "  ++ (show . length) vysledek
    writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml vysledek
    writeVysledek "vrcholy.gpx" vysledek
    writeVysledek "vrcholy-duplicity.gpx" prominentniVrcholy
    writeVysledek "vrcholy-kandidati.gpx" kandidati
    -- writeVysledek "sitka.gpx" body
    putStrLn "KONEC"
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy2.gpx" $ bodyXml body2
    -- writeFile "m:/Dropbox/gc/geokuk/data/vrcholy.gpx" $ bodyXml (take 10000 kandidati)




