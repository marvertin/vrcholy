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
import Gps
import GeonamesTypy

import Control.Concurrent
import qualified Data.Set as S
import Data.Aeson.Lens (key, nth, _String)
--import Data.Aeson.Types (unpack)
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.UTF8 as B
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens
import Data.Text.Encoding
import Data.Maybe
import System.Directory
-- import qualified Data.ByteString.Lazy.Internal as BI

main :: IO ()
main = dotahujGeoNames ggfile2 ggfile3

dotahujGeoNames :: FilePath -> FilePath -> IO ()
dotahujGeoNames fileVrcholy fileGeonames = do
    setLocaleEncoding utf8
    putStrLn $ "Hledani nazvu pres geonames: " ++  fileVrcholy 
    text <- readFile fileVrcholy
    let vrchy = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
    createDirectoryIfMissing True (takeDirectory fileGeonames)
    appendFile fileGeonames "" -- aby případně vznikl
    uzDriveNactenaId <- fmap S.fromList $ nactiGeonamovance fileGeonames
    putStrLn $ "počet již zpracovaných dříve: " ++ (show . S.size) uzDriveNactenaId
    let kopce = vrchy >>= \vrch -> [(Vr, vrVrchol vrch) , (Ks, vrKlicoveSedlo vrch) ]
    nactiVse fileGeonames uzDriveNactenaId kopce  
    putStrLn "KONEC geonames"

nactiVse :: FilePath -> S.Set String -> [(Kotyp, Kopec)] -> IO ()
nactiVse  _ _ [] = return ()
nactiVse  file4geonames uzDriveNactene ((kotyp, kopec) : zbytekKopcu) = do
    identif <- nactiGeoname file4geonames uzDriveNactene kotyp kopec
    nactiVse file4geonames (identif `S.insert` uzDriveNactene) zbytekKopcu

    -- (Vrch {vrVrchol = kopec})
nactiGeoname :: FilePath -> S.Set String -> Kotyp -> Kopec -> IO (String)
nactiGeoname file4geonames uzDriveNactene kotyp kopec  = do
    let gpsKopec@(GpsKopec _ mnm identif) = kopec2gps kopec
    if identif `S.member` uzDriveNactene then do
        putStrLn $  "Uz nacteno: " ++ show kotyp ++ " " ++ identif
      else do
        let url = gpsKopec2url gpsKopec
        body <- provedUspesnyDotaz url
        putStrLn  $ show (S.size uzDriveNactene) ++ ". " ++ show kotyp ++ " " ++ show mnm ++ "   " ++ identif ++ ": " ++  take 100 (show body)
        threadDelay 3000000
        appendFile file4geonames $ show (Georec kotyp mnm identif url (gpsKopec2mapyUrl gpsKopec) body) ++ "\n"
    return identif
   where  
     provedUspesnyDotaz :: String -> IO B.ByteString
     provedUspesnyDotaz url = do
        r <- get url
        let body = r ^. responseBody 
        let bodyGeonames = r ^? responseBody . key "geonames"
        let statuskod = r ^. responseStatus . statusCode
        let opakovat = statuskod /= 200 || isNothing bodyGeonames
        if opakovat then do
           print $  "Opakujeme: " ++ show body
           threadDelay 90000000
           provedUspesnyDotaz url
          else 
           return body   

gpsKopec2url :: GpsKopec -> String
gpsKopec2url (GpsKopec (lat, lng) _ _) = 
    "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat="  ++ show lat ++ "&lng=" ++ show lng

gpsKopec2mapyUrl :: GpsKopec -> String
gpsKopec2mapyUrl (GpsKopec (lat, lng) _ _) = 
    "https://mapy.cz/turisticka?z=16&source=coor"  
    ++ "&x=" ++ show lng ++ "&y=" ++ show lat ++ "&id=" ++ show lng ++ "%2C" ++ show lat
           

nactiGeonamovance :: FilePath -> IO [String]
nactiGeonamovance fileName = do
    txt <- readFile fileName
    return $ map (vyberIdentif . ctiRadek) (lines txt)   
    where 
        ctiRadek :: String -> Georec
        ctiRadek = read 
        vyberIdentif (Georec _ _ identif _ _ _) = identif

           
