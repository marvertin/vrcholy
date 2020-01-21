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
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens
import Data.Text.Encoding


-- import qualified Data.ByteString.Lazy.Internal as BI

main :: IO ()
main = do
    setLocaleEncoding utf8
    soubory <- listDirectory dir3vrcholy
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    forM_ soubory $ \fileName -> do
        putStrLn $ "Hledani nazvu pres geonames: " ++  (dir3vrcholy </> fileName) 
        text <- readFile (dir3vrcholy </> fileName) 
        let vrchy = take 5 $ map read (lines text) :: [Vrch]
        putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
        createDirectoryIfMissing True dir4geonames
        let file4geonames = dir4geonames </> "geonames.txt"
        appendFile file4geonames "" -- aby případně vznikl
        uzDriveNactenaId <- fmap S.fromList $ nactiGeonamovance file4geonames
        forM_ vrchy $ \vrch -> do
            nactiGeoname file4geonames uzDriveNactenaId vrch

nactiGeonamovance :: FilePath -> IO [String]
nactiGeonamovance fileName = do
    txt <- readFile fileName
    return $ map (vyberIdentif . ctiRadek) (lines txt)   
   where 
       ctiRadek :: String -> (Int, String, String, String, Vrch) 
       ctiRadek = read 
       vyberIdentif (_, identif, _, _, _) = identif

gpsKopec2url :: GpsKopec -> String
gpsKopec2url (GpsKopec (lat, lng) _) = 
     "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1&lat="  ++ show lat ++ "&lng=" ++ show lng

nactiGeoname :: FilePath -> S.Set String -> Vrch -> IO()
nactiGeoname file4geonames uzDriveNactene vrch@(Vrch {vrVrchol = kopec}) = do
    let gpsKopec@(GpsKopec _ mnm) = toGps kopec
    let identif = toIdentif kopec
    unless (identif `S.member` uzDriveNactene) $ do
        let url = gpsKopec2url gpsKopec
        r <- get url
        let body = r ^. responseBody 
        print $  (mnm, identif, body)
        threadDelay 4000000
        appendFile file4geonames $ show (mnm, identif, url, body, vrch) ++ "\n"
 
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
        let vrchy = take 2 $ map read (lines text) :: [Vrch]
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
        appendFile file4geonames $ show (mnm, identif, url, body, vrch) ++ "\n"
      