module GeonamesTypy
    ( Kotyp(..),
      Georec(..),
      readGeodecFile,
      readGeodecFileAsMap
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M

-- Kopce typ může být vrchol, klíčové sedlo nebo mateřský vrchol
data Kotyp = Vr | Ks | Mv
 deriving (Show, Read)
-- záznam načtených geodat 
--  typ - mnm - identif - url geonames - url mapycz - data jako json
data Georec = Georec Kotyp Int String String String B.ByteString
 deriving (Show, Read)


readGeodecFile :: FilePath -> IO [Georec] 
readGeodecFile fileName = do
    txt <- readFile fileName
    return $ map ctiRadek (lines txt)   
    where 
        ctiRadek :: String -> Georec
        ctiRadek = read 


readGeodecFileAsMap :: FilePath -> IO (M.Map String B.ByteString)
readGeodecFileAsMap fileName = do
  recs <- readGeodecFile fileName
  return . M.fromList . fmap (\ (Georec _ _ identif _ _ jsontext) -> (identif, jsontext)) $ recs


grJson :: Georec -> B.ByteString
grJson rec = let (Georec _ _ _ _ _ jsontext) = rec
             in jsontext