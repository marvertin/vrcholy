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

-- Kopce typ může být vrchol, klíčové sedlo nebo mateřský vrchol
data Kotyp = Vr | Ks | Mv
 deriving (Show, Read)
-- záznam načtených geodat 
--  typ - mnm - identif - url geonames - url mapycz - data jako json
data Georec = Georec Kotyp Int String String String B.ByteString
 deriving (Show, Read)

-- import qualified Data.ByteString.Lazy.Internal as BI

main :: IO ()
main = do
    setLocaleEncoding utf8
    soubory <- listDirectory dir3vrcholy
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    forM_ soubory $ \fileName -> do
        putStrLn $ "Hledani nazvu pres geonames: " ++  (dir3vrcholy </> fileName) 
        text <- readFile (dir3vrcholy </> fileName) 
        let vrchy = map read (lines text) :: [Vrch]
        putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
        createDirectoryIfMissing True dir4geonames
        let file4geonames = dir4geonames </> "geonames.txt"
        appendFile file4geonames "" -- aby případně vznikl
        uzDriveNactenaId <- fmap S.fromList $ nactiGeonamovance file4geonames
        putStrLn $ "počet již zpracovaných dříve: " ++ (show . S.size) uzDriveNactenaId
        let kopce = vrchy >>= \vrch -> [(Vr, vrVrchol vrch) , (Ks, vrKlicoveSedlo vrch)]
--        forM_ vrchy $ \vrch -> do
            -- nactiGeoname file4geonames uzDriveNactenaId Vr (vrVrchol vrch) 
        nactiVse file4geonames uzDriveNactenaId kopce  

nactiVse :: FilePath -> S.Set String -> [(Kotyp, Kopec)] -> IO ()
nactiVse  file4geonames uzDriveNactene ((kotyp, kopec) : zbytekKopcu) = do
    identif <- nactiGeoname file4geonames uzDriveNactene kotyp kopec
    nactiVse file4geonames (identif `S.insert` uzDriveNactene) zbytekKopcu

    -- (Vrch {vrVrchol = kopec})
nactiGeoname :: FilePath -> S.Set String -> Kotyp -> Kopec -> IO (String)
nactiGeoname file4geonames uzDriveNactene kotyp kopec  = do
    let gpsKopec@(GpsKopec _ mnm) = toGps kopec
    let identif = toIdentif kopec
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
gpsKopec2url (GpsKopec (lat, lng) _) = 
    "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat="  ++ show lat ++ "&lng=" ++ show lng

gpsKopec2mapyUrl :: GpsKopec -> String
gpsKopec2mapyUrl (GpsKopec (lat, lng) _) = 
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

           
migr2nacti :: FilePath -> IO [Georec]
migr2nacti fileName = do
    txt <- readFile fileName
    return $ map (prevod . ctiRadek) (lines txt)   
    where 
        ctiRadek :: String -> (Int, String, String, String, B.ByteString, Vrch) 
        ctiRadek = read 
        prevod (mnm, identif, url1, url2, jsons, _) = Georec Vr mnm identif url1 url2 jsons

migr2 :: IO ()
migr2 = do
    let vstup = dir4geonames </> "geonames.txt"
    let vystup = dir4geonames </> "1geonames.txt"
    grecy <- migr2nacti vstup
    writeFile vystup . unlines . map show $ grecy
           
 
migrNacti :: FilePath -> IO (M.Map String String)
migrNacti fileName = do
    txt <- readFile fileName
    return $ M.fromList (map ctiRadek (lines txt))
   where 
       ctiRadek :: String -> (String, String) 
       ctiRadek = read 
       
       
migruj :: IO()        
migruj = do
    let vstup = dir4geonames </> "0geonames.txt"
    let vystup = dir4geonames </> "1geonames.txt"
    setLocaleEncoding utf8
    soubory <- listDirectory dir3vrcholy
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    forM_ soubory $ \fileName -> do
        putStrLn $ "MIGRACE: " ++  (dir3vrcholy </> fileName) 
        text <- readFile (dir3vrcholy </> fileName) 
        let vrchy = map read (lines text) :: [Vrch]
        putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
        createDirectoryIfMissing True dir4geonames
        let file4geonames = dir4geonames </> "1geonames.txt"
        appendFile file4geonames "" -- aby případně vznikl
        uzDriveNactenaId <- migrNacti vstup
        forM_ vrchy $ \vrch -> do
            migrNactiGeoname file4geonames uzDriveNactenaId vrch

migrNactiGeoname :: FilePath -> M.Map String String -> Vrch -> IO()
migrNactiGeoname file4geonames uzDriveNamovane vrch@(Vrch {vrVrchol = kopec}) = do
    let gpsKopec@(GpsKopec _ mnm) = toGps kopec
    let identif = toIdentif kopec
    when (identif `M.member` uzDriveNamovane) $ do
        let url = gpsKopec2url gpsKopec
        r <- get url
        let (Just body) =  identif `M.lookup` uzDriveNamovane
        print $  (mnm, identif, body)
        appendFile file4geonames $ show (mnm, identif, url, gpsKopec2mapyUrl gpsKopec, body, vrch) ++ "\n"
      