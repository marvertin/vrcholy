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
import Text.Printf (printf)
import Data.List
import Data.Function

hcGps = (49.2839519 :: Double, 16.3563408 :: Double)

data GpsKopec = GpsKopec (Double, Double) Int
  deriving (Show)

-- vzdálenost, prominence, vrchol, klíčové sedlo mateřský vrchol
data GpsVrch = GpsVrch Int GpsKopec GpsKopec GpsKopec 
  deriving (Show)

  -- Kvocient převodu na GPS souřadnice
kvoc = 1.0 / 1200 :: Double;

cas = "2015-01-01T00:00:00.000"

hlavicka = [i|<?xml version="1.0" encoding="utf-8" standalone="no"?>
<gpx creator="Vrcholy haskell program" version="1.1" 
xmlns="http://www.topografix.com/GPX/1/1" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xmlns:gpxx="http://www.garmin.com/xmlschemas/GpxExtensions/v3" 
xmlns:gpxg="https://www.geoget.cz/GpxExtensions/v2"
xmlns:groundspeak="http://www.groundspeak.com/cache/1/0"
xsi:schemaLocation="https://www.geoget.cz/GpxExtensions/v2 https://www.geoget.cz/GpxExtensions/v2/geoget.xsd 
                    http://www.groundspeak.com/cache/1/0 http://www.groundspeak.com/cache/1/0/cache.xsd 
                    http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd 
                    http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd">
<metadata>
  <desc>Vrcholy, jejich klíčová sedla a mateřské island vrcholy</desc>
  <time>#{cas}</time>
</metadata>
                 
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

vzdalenost2identif :: Int -> String
vzdalenost2identif vzdalenost = printf "%04dK%03d" (vzdalenost `div` 1000) (vzdalenost `mod` 1000)


bodXml :: ((Int, Int), GpsVrch) -> String
bodXml ((poradi, vzdalenost), vrch) = 
 let (GpsVrch prominence (GpsKopec (vrlat, vrlon) vrele) 
                         (GpsKopec (kslat, kslon) ksele) 
                         (GpsKopec (mvlat, mvlon) mvele) 
                               ) =  vrch 
     nazev = show vrele
     identif = vzdalenost2identif vzdalenost
 in [i|

<wpt lat="#{vrlat}" lon="#{vrlon}">
<time>#{cas}</time>
<name>VRCH#{identif}</name>
<sym>Geocache</sym>
<type>Geocache|Project APE Cache</type>
<extensions>
 <groundspeak:cache>
    <!-- Zobrazí se v názvu vrcholu, musí to být tady, nestačí desc či něco jiného -->
    <groundspeak:name>#{nazev}</groundspeak:name>
    <groundspeak:placed_by>-</groundspeak:placed_by>
    <groundspeak:container>Other</groundspeak:container>
 </groundspeak:cache>

 <gpxg:GeogetExtension>
   <gpxg:Tags>
     <gpxg:Tag Category="poradi">#{poradi}</gpxg:Tag>
     <gpxg:Tag Category="vzdalenost">#{vzdalenost}</gpxg:Tag>
     <gpxg:Tag Category="prominence">#{prominence}</gpxg:Tag>
     <gpxg:Tag Category="elevation2">#{vrele}</gpxg:Tag>
   </gpxg:Tags>
 </gpxg:GeogetExtension>
</extensions>
</wpt>

<wpt lat="#{kslat}" lon="#{kslon}">
<time>#{cas}</time>
<name>KSCH#{identif}</name>

<!-- Zobrazí se v názvu dodatečného bodu -->
<desc>Klíčové sedlo k #{nazev}</desc>
<!-- Zajišťuje, že se přiváže k nadřízenému bodu -->
<type>Waypoint|Reference Point</type>
</wpt>

<wpt lat="#{mvlat}" lon="#{mvlon}">
<time>#{cas}</time>
<name>MVCH#{identif}</name>
<desc>Mateřský vrchol k #{nazev}</desc>
<type>Waypoint|Reference Point</type>
</wpt>
|]


-- 49.2839519N, 16.3563408E
bodyXml :: [Vrch] -> String
bodyXml vrchy = 
                let 
                    sezSeVzdalenosti = map (\ gpp@(GpsVrch _ (GpsKopec sou _) _ _) -> (round (distance2 hcGps sou), gpp)) 
                      . map prevod $ vrchy
                    sezSortedDleVysek = reverse $ sortBy (compare `on` dejVysku) sezSeVzdalenosti
                    (vzdalenosti, vrchy) = unzip sezSortedDleVysek
                    sez2 = zip (zip [1..] vzdalenosti) vrchy -- řazení: výšky, obsahuje vzdálenost i pořadí dle výšek
                    sez3 = sortBy (compare `on` (snd.fst)) sez2 -- řazení: vzdálenosti

                    sez = let -- rozbalit a zabalit, aby se udelali vzdalenosti unikatni
                            (poradiAVzdalenosti2, vrchy2) = unzip sez3
                            (poradi2, vzdalenosti2) = unzip poradiAVzdalenosti2
                          in zip (zip poradi2 (rostouci vzdalenosti2)) vrchy2 -- opraveny seznam, aby vzdalenosti byly unikatni

                    vrchyJakoXmlStr :: [String]
                    vrchyJakoXmlStr = map bodXml sez
                          
                in hlavicka ++ (concat (vrchyJakoXmlStr) ++ paticka           
        where dejVysku (_, (GpsVrch _ (GpsKopec _ mnm) _ _  )) = mnm

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (lat1 , lon1) (lat2 , lon2) = sqrt (lat'*lat' + lon'*lon')
    where lat' = (lat1 - lat2) * 60 * 1852
          lon' = (lon1 - lon2) * 60 * 1852 * (lat1 / 180 * pi)
        