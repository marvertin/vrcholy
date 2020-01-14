{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Zapisovac
    (   bodyXml
    ) where
 
import Lib
import Uzemi
import Control.Arrow
import VrchTypy(Vrch(..), Kopec(..))
import Data.String.Interpolate ( i )

hcGps = (49.2839519 :: Double, 16.3563408 :: Double)

data GpsKopec = GpsKopec (Double, Double) Int
  deriving (Show)

-- vzdálenost, prominence, vrchol, klíčové sedlo mateřský vrchol
data GpsVrch = GpsVrch Int GpsKopec GpsKopec GpsKopec 
  deriving (Show)

  -- Kvocient převodu na GPS souřadnice
kvoc = 1.0 / 1200 :: Double;

hlavicka = [i|<?xml version="1.0" encoding="utf-8" standalone="no"?>
<gpx xmlns="http://www.topografix.com/GPX/1/1" creator="Vrcholy haskell program" version="1.1" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xmlns:gpxx="http://www.garmin.com/xmlschemas/GpxExtensions/v3" 
xsi:schemaLocation="https://www.geoget.cz/GpxExtensions/v2 https://www.geoget.cz/GpxExtensions/v2/geoget.xsd 
                    http://www.groundspeak.com/cache/1/0 http://www.groundspeak.com/cache/1/0/cache.xsd 
                    http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd 
                    http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd">

|]

paticka = [i|
</gpx>
|]

vystred  :: (Mou -> Int) -> [Mou]  -> Double
vystred _ [] = -1
vystred fn mous = (sum (map (\mou -> fromIntegral (fn mou) * kvoc) mous)  ) / fromIntegral (length mous)

toGps :: Kopec -> GpsKopec
toGps (Kopec mnm (Moustrov mous)) = GpsKopec (vystred yy &&& vystred xx $ mous) mnm

prevod :: Vrch -> GpsVrch
prevod (Vrch { vrVrchol = vrVrchol@(Kopec mnmVrch _), 
              vrKlicoveSedlo = vrKlicoveSedlo@(Kopec mnmSedlo _),
              vrMaterskeVrcholy } ) =
                let vrcholGps@(GpsKopec sou _) = toGps vrVrchol
                in  GpsVrch  (mnmVrch - mnmSedlo) vrcholGps (toGps vrKlicoveSedlo) (toGps vrMaterskeVrcholy)

                -- (round (distance2 hcGps sou))


bodXml :: (Int, (Int, GpsVrch)) -> String
bodXml (poradi, (vzdalenost, vrch)) = 
 let (GpsVrch prominence (GpsKopec (vrlat, vrlon) vrele)  _ _ ) =  vrch 
     desc = show vrele
     name = "VR_" ++ show vzdalenost ++ "_" ++ show poradi
 in [i|
<wpt lat="#{vrlat}" lon="#{vrlon}">
  <name>#{name}</name>
  <desc>#{desc}</desc>
  <sym>SymVrchol</sym>
  <type>TypeVrchol</type>
  <elevation>#{vrele}</elevation>
</wpt>
|]


-- 49.2839519N, 16.3563408E
bodyXml :: [Vrch] -> String
bodyXml vrchy = 
    let vrchyJakoXmlStr :: [String]
        vrchyJakoXmlStr =
         map bodXml 
          (zip [1..]
               (map (\ gpp@(GpsVrch _ (GpsKopec sou _) _ _) -> (round (distance2 hcGps sou), gpp))
                 . map prevod $ vrchy))
    in hlavicka ++ (concat vrchyJakoXmlStr) ++ paticka           


distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (lat1 , lon1) (lat2 , lon2) = sqrt (lat'*lat' + lon'*lon')
    where lat' = (lat1 - lat2) * 60 * 1852
          lon' = (lon1 - lon2) * 60 * 1852 * (lat1 / 180 * pi)
        