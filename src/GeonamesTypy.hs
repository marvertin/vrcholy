module GeonamesTypy
    ( Kotyp(..),
      Georec(..),
      readGeorecDir,
      readGeorecDirAsMap,
      readGeorecDirAsIdentifs
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Lib

-- Kopce typ může být vrchol, klíčové sedlo nebo mateřský vrchol
data Kotyp = Vr | Ks | Mv
 deriving (Show, Read)
-- záznam načtených geodat 
--  typ - mnm - identif - url geonames - url mapycz - data jako json
data Georec = Georec Kotyp Int String String String B.ByteString
 deriving (Show, Read)


readGeorecDir :: FilePath -> IO [Georec] 
readGeorecDir dirName = do
    fmap (map ctiRadek) . readLinesFromDir $ dirName
    where 
        ctiRadek :: String -> Georec
        ctiRadek = read 


readGeorecDirAsMap :: FilePath -> IO (M.Map String B.ByteString)
readGeorecDirAsMap dirName = do
  recs <- readGeorecDir dirName
  return . M.fromList . fmap (\ (Georec _ _ identif _ _ jsontext) -> (identif, jsontext)) $ recs

readGeorecDirAsIdentifs :: FilePath -> IO (S.Set String)
readGeorecDirAsIdentifs dirName = do
  recs <- readGeorecDir dirName
  return . S.fromList . fmap (\ (Georec _ _ identif _ _ _) -> identif) $ recs
  
  
  

grJson :: Georec -> B.ByteString
grJson rec = let (Georec _ _ _ _ _ jsontext) = rec
             in jsontext