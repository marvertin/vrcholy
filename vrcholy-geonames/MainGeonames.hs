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
        let vrchy = map read (lines text) :: [Vrch]
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
    return $ map (fst . ctiRadek) (lines txt)   
   where 
       ctiRadek :: String -> (String, String) 
       ctiRadek = read 

nactiGeoname :: FilePath -> S.Set String -> Vrch -> IO()
nactiGeoname file4geonames uzDriveNactene (Vrch {vrVrchol = kopec}) = do
    let (GpsKopec (lat, lng) _) = toGps kopec
    let identif = toIdentif kopec
    unless (identif `S.member` uzDriveNactene) $ do
        let opts = defaults & param "lat" .~ [T.pack (show lat)] & param "lng" .~ [T.pack (show lng)] 
        r <- getWith opts "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=5&radius=1"
        let body = r ^. responseBody 
        print $  (identif, body)
        threadDelay 4000000
        appendFile file4geonames $ show (identif, body) ++ "\n"
        
       

zprac :: B.ByteString -> IO ()
zprac hh = do
    let h1 = show hh
    let h2 = read h1 :: String
    putStrLn $  h2

p :: IO ()
p = do
    let lat = 49.2854672 :: Double
    let lng = 16.4719033 :: Double

    let opts = defaults & param "lat" .~ [T.pack (show lat)] & param "lng" .~ [T.pack (show lng)] 
    r2 <- getWith opts "http://api.geonames.org/findNearbyJSON?username=marvertin&verbosity=FULL&maxRows=100&radius=1"
   -- print r2
    let rs = show r2
    -- let r = read rs :: Response BI.ByteString 
 --   let r = r2 :: Response BI.ByteString 
    let r = r2
    -- let typ = typeOf r
    --let r = read
    let statuskod = r ^. responseStatus . statusCode
    print statuskod
    --let pole = r ^? responseBody . key "geonames" . traverseArray 
    -- let ss = unpack pole
    -- print pole
    let body = r ^. responseBody 
    zprac body
    --print $ typeOf body2
    let tono0x = body ^. key "geonames" . nth 0 . key "toponymName" . _String
    let bodys = show body
    -- print tono0x
    putStrLn $ "BODYS: " ++ bodys
    --putStrLn $ "BODYS: " ++ body
--    let body2 = read bodys
--    print body2
--    putStrLn $ B8.toString body
    let tono0 = r ^. responseBody . key "geonames" . nth 0 . key "toponymName" . _String
    --print tono0
    let tono1 = r ^. responseBody . key "geonames" . nth 1 . key "toponymName" . _String
    --print tono1
    let tono2 = r ^. responseBody . key "geonames" . nth 2 . key "toponymName" . _String
    --print tono2
    let tono3 = r ^. responseBody . key "geonames" . nth 2 . key "toponymName" . _String
    --print tono3
    return ()

    
