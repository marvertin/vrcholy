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
import  Control.Monad



writeVysledekV :: String -> [Vrch] -> IO()
writeVysledekV filename body = do
    let fullFn =  dir4gpx </> filename
--    toto čerpá strašně moc ramky    
--    putStrLn $ "Zapis "  ++ (show . length) body ++ " bodu do \"" ++ fullFn ++ "\""
    writeFile fullFn (bodyXml body)

main :: IO ()
main = do
    soubory <- listDirectory dir3vrcholy
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    forM_ soubory $ \fileName -> do
        putStrLn $ "Prevod do GPX: " ++  (dir3vrcholy </> fileName) 
        text <- readFile (dir3vrcholy </> fileName) 
        let vrchy = map read (lines text) :: [Vrch]
        putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
        createDirectoryIfMissing True dir4gpx
        writeVysledekV (fileName ++ ".gpx") vrchy
    
