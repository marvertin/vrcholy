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
import GeonamesIo
import GeonamesParser

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
import Data.Sort
import System.IO.Error 
import System.IO
import Control.Exception
import Network.HTTP.Client (HttpException)

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
        putStrLn  $ show (S.size uzDriveNactene) ++ ". " ++ show kotyp ++ " " ++ show mnm ++ "   " ++ identif ++ ": " 
          ++  show (dejNazev body) ++ " | " ++ take 100 (show body)
        threadDelay 3000000
        appendOneGeorec dirGeonames (Georec kotyp mnm identif url (gpsKopec2mapyUrl gpsKopec) body)
    return identif
   where  
    provedUspesnyDotaz :: String -> IO B.ByteString
    provedUspesnyDotaz url = (do
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
       )  `catch` excHandler
      where 
       excHandler :: HttpException -> IO B.ByteString
       excHandler e = do
           putStrLn $ "Nejde to: " ++ show e
           threadDelay $ 300 * 1000000
           provedUspesnyDotaz url
       
    
gpsKopec2url :: GpsKopec -> String
gpsKopec2url (GpsKopec (lat, lng) _ _) = 
    "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat="  ++ show lat ++ "&lng=" ++ show lng

gpsKopec2mapyUrl :: GpsKopec -> String
gpsKopec2mapyUrl (GpsKopec (lat, lng) _ _) = 
    "https://mapy.cz/turisticka?z=16&source=coor"  
    ++ "&x=" ++ show lng ++ "&y=" ++ show lat ++ "&id=" ++ show lng ++ "%2C" ++ show lat
           
-- rozdělní soubory v adresáři geonamů podle jmeno (bylo potřeba při migraci)
rozdel srcDir destDir = do
    recs <- readGeorecDir srcDir
    forM_ recs $ appendOneGeorec destDir

rozd = rozdel ggdir3 "M:/vrch-CZ/novegeonames"