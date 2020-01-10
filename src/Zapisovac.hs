{-# LANGUAGE QuasiQuotes #-}

module Zapisovac
    (   bodyXml
    ) where
 
import Lib
import Uzemi
import VrchTypy(Vrch(..), Kopec(..))
import Data.String.Interpolate ( i )

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

vystred  :: [Mou] -> (Mou -> Int) -> Double
vystred mous fn = (sum (map (\mou -> fromIntegral (fn mou) * kvoc) mous)  ) / fromIntegral (length mous)

bodXml :: Vrch -> String
bodXml  Vrch { vrVrchol = Kopec  elevation (Moustrov mous), vrKlicoveSedlo = Kopec mnmSedlo _ } = 

--bodXml ((x,y), elevation) = 
 let latitude = vystred mous snd
     longitude =  vystred mous fst
     (x,y) = vystredMou mous
     desc = show elevation ++ " (" ++ show (elevation - mnmSedlo)  ++ ")"
     name = "VR_" ++ show x ++ "_" ++ show y
 in [i|
<wpt lat="#{latitude}" lon="#{longitude}">
  <name>#{name}</name>
  <desc>#{desc}</desc>
  <sym>SymVrchol</sym>
  <type>TypeVrchol</type>
  <elevation>#{elevation}</elevation>
</wpt>
|]

-- 49.2839519N, 16.3563408E
bodyXml :: [Vrch] -> String
bodyXml body = hlavicka ++ (concat (map bodXml body)) ++ paticka
