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
import GeonamesTypy

import Data.Aeson.Lens
--import Data.Aeson.Types (unpack)
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.UTF8 as B8
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens

main :: IO ()
main = vyrobGpx ggfile2 ggfile3 ggfile4

vyrobGpx  :: FilePath -> FilePath -> FilePath -> IO ()
vyrobGpx file3Vrcholy file4Geonames file5Gpx = do
    setLocaleEncoding utf8
    fceNazev <- identif2nazev file4Geonames dejNazev
    putStrLn $ "Prevod do GPX: " ++  file3Vrcholy
    text <- readFile file3Vrcholy
    let vrchy = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
    createDirectoryIfMissing True (takeDirectory file5Gpx)
    writeFile file5Gpx (bodyXml fceNazev vrchy)
    
-- vezme funkci, která z json dat vybere jméno
-- vrátí funkci, která z identifikátoru udělá jméno prohnáním přes načtenou mapu
identif2nazev :: FilePath -> (B.ByteString -> Maybe String) -> IO (String -> Maybe String)
identif2nazev fileName fce = do
    putStrLn $ "Ctu geodec data: "  ++ fileName
    souborExistuje <- doesFileExist fileName 
    mapa <- if souborExistuje then readGeodecFileAsMap fileName else return M.empty
    putStrLn $ "Precteno "  ++ (show.M.size) mapa ++ " geonames" 
    return $ \identif ->
        mapa M.!? identif >>= fce



p :: IO ()
p = do
    let lat = 49.2854672 :: Double
    let lng = 16.4719033 :: Double

    let opts = defaults & param "lat" .~ [T.pack (show lat)] & param "lng" .~ [T.pack (show lng)] 
    r <- getWith opts "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=100&radius=1"
    -- print r
    let statuskod = r ^. responseStatus . statusCode
    print statuskod
    --let pole = r ^? responseBody . key "geonames" . traverseArray 
    -- let ss = unpack pole
    -- print pole
--    let body = r ^. responseBody
--    putStrLn $ B8.toString body
    let tono0 = r ^. responseBody . key "geonames" . nth 0 . key "toponymName" . _String
    print tono0
    let tono1 = r ^. responseBody . key "geonames" . nth 1 . key "toponymName" . _String
    print tono1
    let tono2 = r ^. responseBody . key "geonames" . nth 2 . key "toponymName" . _String
    print tono2
    let tono3 = r ^. responseBody . key "geonames" . nth 2 . key "toponymName" . _String
    print tono3

    

