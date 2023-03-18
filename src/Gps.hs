{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gps
    (   
       GpsVrch(..), GpsKopec(..), 
       hcGps,
       kopec2gps, 
       vrch2gps,
       boduNaStupen
    ) where
 
import Lib
import Uzemi
import VrchTypy
import Control.Arrow
import Data.String.Interpolate ( i )
import Text.Printf (printf)
import Data.List
import Data.Function

hcGps = (49.2839519 :: Double, 16.3563408 :: Double)

-- Kolik bodů máme v Mou souřanicích na zeměpisný stupeň
boduNaStupenInt = 1200 :: Int
boduNaStupen = 1200 :: Integer
boduNaStupenD = fromIntegral boduNaStupen

type Identif = String 

data GpsKopec = GpsKopec (Double, Double) Int Identif
  deriving (Show)

--  prominence, vrchol, klíčové sedlo mateřský vrchol
data GpsVrch = GpsVrch Int GpsKopec GpsKopec GpsKopec 
  deriving (Show)

  -- Kvocient převodu na GPS souřadnice
kvoc = 1.0 / boduNaStupenD :: Double;

-- rozsahy jsou: x: <-180 * boduNaStupen : 180 * boduNaStupen>
--               y: <-80 * boduNaStupen : 80 * boduNaStupen>
mouToId :: Mou -> String
mouToId (Mou x y) =
  let  x' = fromIntegral $ (x + ((360 + 15) * boduNaStupenInt)) `mod` (360 * boduNaStupenInt)
       y' = fromIntegral $ (y + (180 * boduNaStupenInt)) `mod` (180 * boduNaStupenInt)
  in base34 $ x' * (180 * boduNaStupen) + y' + 1*34*34*34*34*34*34

ee x y = mouToId $ Mou (x * boduNaStupenInt) (y * boduNaStupenInt)

mousToId :: [Mou] -> String
mousToId mous = mouToId $ Mou (ivystr xx mous) (ivystr yy mous) 

ivystr :: (Mou -> Int) -> [Mou]  -> Int
ivystr _ [] = -1
ivystr fn mous = (sum $ map fn mous) `div` length mous
  
vystred  :: (Mou -> Int) -> [Mou]  -> Double
vystred _ [] = -1
vystred fn mous = (sum (map (\mou -> fromIntegral (fn mou) * kvoc) mous)  ) / fromIntegral (length mous)

kopec2gps :: Kopec -> GpsKopec
kopec2gps (Kopec mnm (Moustrov mous)) = GpsKopec (vystred yy &&& vystred xx $ mous) mnm (mousToId mous)

vrch2gps :: Vrch -> GpsVrch
vrch2gps (Vrch { vrVrchol = vrVrchol@(Kopec mnmVrch _), 
                 vrKlicoveSedlo = vrKlicoveSedlo@(Kopec mnmSedlo _),
                 vrMaterskeVrcholy } ) =
                 GpsVrch (mnmVrch - mnmSedlo) (kopec2gps vrVrchol) (kopec2gps vrKlicoveSedlo) (kopec2gps vrMaterskeVrcholy)

