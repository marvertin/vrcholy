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
import Data.List.Split
import Data.Char
-- import qualified Data.ByteString.Lazy.Internal as BI

main :: IO ()
main = dotahujGeoNames ggfile2 ggdir3

dotahujGeoNames :: FilePath -> FilePath -> IO ()
dotahujGeoNames fileVrcholy dirGeonames = do
    setLocaleEncoding utf8
    putStrLn $ "Hledani nazvu pres geonames: " ++  fileVrcholy 
    text <- readFile fileVrcholy
    let vrchy = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
    createDirectoryIfMissing True dirGeonames

    uzDriveNactenaId <- readGeorecDirAsIdentifs dirGeonames
    putStrLn $ "počet již zpracovaných dříve: " ++ (show . S.size) uzDriveNactenaId
    let kopce = vrchy >>= \vrch -> [(Vr, vrVrchol vrch) , (Ks, vrKlicoveSedlo vrch) ]
    dotahujVse dirGeonames uzDriveNactenaId kopce  
    putStrLn "KONEC geonames"

dotahujVse :: FilePath -> S.Set String -> [(Kotyp, Kopec)] -> IO ()
dotahujVse  _ _ [] = return ()
dotahujVse  dirGeonames uzDriveNactene ((kotyp, kopec) : zbytekKopcu) = do
    identif <- dotahniGeoname dirGeonames uzDriveNactene kotyp kopec
    dotahujVse dirGeonames (identif `S.insert` uzDriveNactene) zbytekKopcu

dotahniGeoname :: FilePath -> S.Set String -> Kotyp -> Kopec -> IO (String)
dotahniGeoname dirGeonames uzDriveNactene kotyp kopec  = do
    let gpsKopec@(GpsKopec _ mnm identif) = kopec2gps kopec
    if identif `S.member` uzDriveNactene then do
        putStrLn $  "Uz nacteno: " ++ show kotyp ++ " " ++ identif
      else do
        let url = gpsKopec2url gpsKopec
        body <- provedUspesnyDotaz url
        putStrLn  $ show (S.size uzDriveNactene) ++ ". " ++ show kotyp ++ " " ++ show mnm ++ "   " ++ identif ++ ": " ++  take 100 (show body)
        threadDelay 3000000
        ulozGeoname dirGeonames (Georec kotyp mnm identif url (gpsKopec2mapyUrl gpsKopec) body)
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

ulozGeoname :: FilePath -> Georec -> IO ()
ulozGeoname dirGeonames georec@(Georec _ _ _ url _ _) = do
    let soubor = dirGeonames </> url2geonamesFileName url
    appendFile soubor $ show georec ++ "\n"

gpsKopec2url :: GpsKopec -> String
gpsKopec2url (GpsKopec (lat, lng) _ _) = 
    "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat="  ++ show lat ++ "&lng=" ++ show lng

gpsKopec2mapyUrl :: GpsKopec -> String
gpsKopec2mapyUrl (GpsKopec (lat, lng) _ _) = 
    "https://mapy.cz/turisticka?z=16&source=coor"  
    ++ "&x=" ++ show lng ++ "&y=" ++ show lat ++ "&id=" ++ show lng ++ "%2C" ++ show lat
           

url2geonamesFileName :: String -> String
url2geonamesFileName url =
    let [lat, lng] = map (takeWhile isDigitOrMinus) . map (drop 4) . reverse . take 2 . reverse . (splitWhen (=='&')) $ url
    in "geonames-" ++ nahradMinus 'N' 'S' lat ++ nahradMinus 'E' 'W' lng ++ ".txt"

isDigitOrMinus x = isDigit x || x == '-'

nahradMinus :: Char -> Char -> String -> String
nahradMinus zp zm ('-' : s)  = (zm : s)
nahradMinus zp zm s = (zp : s)

qqq = do
    print $ url2geonamesFileName "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat=-49.157500000000006&lng=19.999166666666667"    


