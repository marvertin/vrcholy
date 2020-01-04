{-# LANGUAGE QuasiQuotes #-}

module Zapisovac
    (   x, v, bodyXml
    ) where

import Lib
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

bodXml :: Bod -> String
bodXml ((x,y), elevation) = 
 let latitude = fromIntegral y * kvoc   
     longitude = fromIntegral x * kvoc
     desc = show elevation
     name = "VR" ++ show x ++ show y
 in [i|
<wpt lat="#{latitude}" lon="#{longitude}">
  <name>#{name}</name>
  <desc>#{desc}</desc>
  <sym>SymVrchol</sym>
  <type>TypeVrchol</type>
  <elevation>#{elevation}</elevation>
</wpt>
|]

v = bodyXml [((19627,55984),300), ((19628,55985),1000)]
-- 49.2839519N, 16.3563408E
bodyXml :: [Bod] -> String
bodyXml body = hlavicka ++ (concat (map bodXml body)) ++ paticka
x = do 
    putStrLn $ v
