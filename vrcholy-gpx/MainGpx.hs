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

import Data.Aeson.Lens
--import Data.Aeson.Types (unpack)
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens

writeVysledekV :: String -> [Vrch] -> IO()
writeVysledekV filename body = do
    let fullFn =  dir5gpx </> filename
--    toto čerpá strašně moc ramky    
--    putStrLn $ "Zapis "  ++ (show . length) body ++ " bodu do \"" ++ fullFn ++ "\""
    writeFile fullFn (bodyXml body)

main :: IO ()
main = do
    setLocaleEncoding utf8
    soubory <- listDirectory dir3vrcholy
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    forM_ soubory $ \fileName -> do
        putStrLn $ "Prevod do GPX: " ++  (dir3vrcholy </> fileName) 
        text <- readFile (dir3vrcholy </> fileName) 
        let vrchy = map read (lines text) :: [Vrch]
        putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
        createDirectoryIfMissing True dir5gpx
        writeVysledekV (fileName ++ ".gpx") vrchy
    

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

    

