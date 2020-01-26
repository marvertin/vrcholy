--
-- Modul se postará ukládání na disk a načítáno z disku již existujících geonames.
--
module GeonamesIo
    ( Kotyp(..),
      Georec(..),
      readGeorecDir,
      readGeorecDirAsMap,
      readGeorecDirAsIdentifs,
      appendOneGeorec
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Char
import Data.List.Split
import System.FilePath.Posix

import Lib

-- Kopce typ může být Vrrchol, nebo Sedlo 
-- (historicky je to Ks jako klíčové sedlo, mateřský vrchol od vrcholu zde nemá smysl rozlišovat)
data Kotyp = Vr | Ks
   deriving (Show, Read)

-- záznam načtených geodat 
--  typ - mnm - identif - url geonames - url mapycz - data jako json
data Georec = Georec Kotyp Int String String String B.ByteString
 deriving (Show, Read)

-- Získá všechny záznamy geonames z daného adresáře.
readGeorecDir :: FilePath -> IO [Georec] 
readGeorecDir dirName = do
    fmap (map ctiRadek) . readLinesFromDir $ dirName
    where 
        ctiRadek :: String -> Georec
        ctiRadek = read 

-- Získá všechny záznamy geonames z daného adresáře jako mapu
readGeorecDirAsMap :: FilePath -> IO (M.Map String (Kotyp, B.ByteString))
readGeorecDirAsMap dirName = do
  recs <- readGeorecDir dirName
  return . M.fromList . fmap (\ (Georec kotyp _ identif _ _ jsontext) -> (identif, (kotyp, jsontext))) $ recs

-- Získá všechny záznamy geonames z daného adresáře jako množinu identifikátorů  
readGeorecDirAsIdentifs :: FilePath -> IO (S.Set String)
readGeorecDirAsIdentifs dirName = do
  recs <- readGeorecDir dirName
  return . S.fromList . fmap (\ (Georec _ _ identif _ _ _) -> identif) $ recs
  
-- Zapíše georec do daného adresáře  
-- Přesněji řečeno ho připíše ke správnému záznamu dle souřadnic v url
appendOneGeorec :: FilePath -> Georec -> IO ()
appendOneGeorec dirGeonames georec@(Georec _ _ _ url _ _) = do
    let soubor = dirGeonames </> url2geonamesFileName url
    appendFile soubor $ show georec ++ "\n"
  
url2geonamesFileName :: String -> String
url2geonamesFileName url =
    let [lat, lng] = map (takeWhile isDigitOrMinus) . map (drop 4) . reverse . take 2 . reverse . (splitWhen (=='&')) $ url
    in "geonames-" ++ nahradMinus 'N' 'S' lat ++ nahradMinus 'E' 'W' lng ++ ".txt"
 where
    isDigitOrMinus x = isDigit x || x == '-'

    nahradMinus :: Char -> Char -> String -> String
    nahradMinus zp zm ('-' : s)  = (zm : s)
    nahradMinus zp zm s = (zp : s)

  
-- Získá z uloženého georec vlastí json v binární podobě
grJson :: Georec -> B.ByteString
grJson rec = let (Georec _ _ _ _ _ jsontext) = rec
             in jsontext