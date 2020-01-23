{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gps
    (   
       GpsVrch(..), GpsKopec(..), 
       hcGps,
       kopec2gps, 
       vrch2gps,
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

type Identif = String 

data GpsKopec = GpsKopec (Double, Double) Int Identif
  deriving (Show)

--  prominence, vrchol, klíčové sedlo mateřský vrchol
data GpsVrch = GpsVrch Int GpsKopec GpsKopec GpsKopec 
  deriving (Show)

  -- Kvocient převodu na GPS souřadnice
kvoc = 1.0 / 1200 :: Double;

-- rozsahy jsou: x: <-180 * 1200 : 180 * 1200>
--               y: <-80 * 1200 : 80 * 1200>
mouToId :: Mou -> String
mouToId (Mou x y) =
  let  x' = fromIntegral $ (x + ((360 + 15) * 1200)) `mod` (360 * 1200)
       y' = fromIntegral $ (y + (180 * 1200)) `mod` (180 * 1200)
  in base34 $ x' * (180 * 1200) + y' + 1*34*34*34*34*34*34

ee x y = mouToId $ Mou (x * 1200) (y * 1200)

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

