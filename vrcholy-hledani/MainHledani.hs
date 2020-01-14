module Main where

import Lib
import Nacitac
import Zapisovac
import Hledac
import Potopa
import Uzemi
import VrchTypy
import PripravaVstupu

import System.FilePath.Posix
import Konst
import System.Directory
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import  Control.Arrow


minimalniProminence = 10


writeVysledekV :: String -> [Vrch] -> IO()
writeVysledekV filename body = do
    let fullFn =  dir3vrcholy ++ filename
--    toto čerpá strašně moc ramky    
--    putStrLn $ "Zapis "  ++ (show . length) body ++ " bodu do \"" ++ fullFn ++ "\""
    writeFile fullFn (bodyXml body)

main :: IO ()
-- main = potopaSvetaZTransponovanychSrtm
-- main = potopaSvetaPrimoZeSrtmDlazdic
main = potopaSvetaZTransponovanychSrtm

potopaSvetaZTransponovanychSrtm :: IO ()
potopaSvetaZTransponovanychSrtm = do
    putStrLn $ "Potopa sveta primo z transponovanych srtm"
    hladiny <- loadAll dir2srtm
    let vrcholy = potopaSveta minimalniProminence hladiny
    --putStrLn $ unlines $ map show vrcholy
    createDirectoryIfMissing True dir3vrcholy
    writeFile (dir3vrcholy </> "vrcholy.vrch") $ unlines $ map show vrcholy
    putStrLn "KONEC"

hledaniVrcholuAZapisVysledku :: String -> (Mnm -> [Bod] -> [Vrch])  -> IO ()
hledaniVrcholuAZapisVysledku vystupniSoubor hledaciFce = do
    soubory <- listDirectory dir1srtm
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory

    obsahy <- mapM B.readFile $ map ( dir1srtm </> ) soubory
    let souradky = map fileNameToCoord soubory 

    let body = loadSrtms (zip souradky obsahy)
    putStrLn $ "Pocet bodu:        " ++  (show . length) body
    let vrcholy = hledaciFce minimalniProminence body
    putStrLn ""
    --putStrLn $ "Pocet vrcholu:     " ++  (show . length) vrcholy
    createDirectoryIfMissing True dir3vrcholy
    writeFile (dir3vrcholy </> vystupniSoubor) $ unlines $ map show vrcholy
    -- writeVysledekV "vrcholy.gpx" vrcholy
    putStrLn "KONEC"

potopaSvetaPrimoZeSrtmDlazdic :: IO ()
potopaSvetaPrimoZeSrtmDlazdic = do
    putStrLn $ "Potopa sveta primo z bodu"
    hledaniVrcholuAZapisVysledku "vrcholy-prima-potopa.vrch" potopaSvetaZBodu

primeHledaniVrcholuZeSrtmDlazdic :: IO () 
primeHledaniVrcholuZeSrtmDlazdic = do
    putStrLn $ "Prime hledani vrcholu ze srtm dlazdic."
    hledaniVrcholuAZapisVysledku "vrcholy-primo.vrch" najdiVrcholyZBodu

