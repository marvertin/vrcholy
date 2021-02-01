{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Nacitac
import Zapisovac
import Hledac
import Potopa
import Uzemi
import VrchTypy
import PripravaVstupu
import Konst
import GeonamesParser
import JsonParser
import GeonamesIo

import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens
import Control.Monad

main :: IO ()
main = join $ liftM3 vyrobGpx ggfile2 ggdir3 ggfile4gpx

vyrobGpx  :: FilePath -> FilePath -> FilePath -> IO ()
vyrobGpx file2Vrcholy dir3Geonames file4Gpx = do
    setLocaleEncoding utf8
    fceNazev <- identif2nazev dir3Geonames dejNazev
    putStrLn $ "Prevod do GPX: " ++  file2Vrcholy
    text <- readFile file2Vrcholy
    let vrchy = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
    createDirectoryIfMissing True (takeDirectory file4Gpx)
    writeFile file4Gpx (bodyXml fceNazev vrchy)
    
-- vezme funkci, která z json dat vybere jméno
-- vrátí funkci, která z identifikátoru udělá jméno prohnáním přes načtenou mapu
identif2nazev :: FilePath -> (Kotyp -> B.ByteString -> Maybe String) -> IO (String -> Maybe String)
identif2nazev dirName fce = do
    putStrLn $ "Ctu geodec data: "  ++ dirName
    adresarExistuje <- doesDirectoryExist dirName 
    mapa <- if adresarExistuje then readGeorecDirAsMap dirName else return M.empty
    putStrLn $ "Precteno "  ++ (show.M.size) mapa ++ " geonames" 
    return $ \identif ->
        mapa M.!? identif >>= uncurry fce


