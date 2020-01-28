{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Zapisovac
    (   bodyXml
    ) where
 
import Lib
import Uzemi
import Gps
import Control.Arrow
import VrchTypy(Vrch(..), Kopec(..))
import Data.String.Interpolate ( i )
import Text.Printf (printf)
import Data.List
import Data.Function
import Data.Maybe


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



bodXml :: (String -> Maybe String) -> ((Int, Integer), GpsVrch) -> String
bodXml nazevFce ((poradi, vzdalenost), vrch) = 
 let (GpsVrch prominence (GpsKopec (vrlat, vrlon) vrele vridentif) 
                         (GpsKopec (kslat, kslon) ksele ksidentif) 
                         (GpsKopec (mvlat, mvlon) mvele mvidentif) 
                               ) =  vrch 
     vrnazev = fromMaybe (show vrele) (nazevFce vridentif)
     ksnazev = fromMaybe "" . fmap (++ " - ") . nazevFce $ ksidentif
     mvnazev = fromMaybe "" . fmap (++ " - ") . nazevFce $ mvidentif
     
 in [i|

<wpt lat="#{vrlat}" lon="#{vrlon}">
<time>#{cas}</time>
<name>VRV#{vridentif}</name>
<sym>Geocache</sym>
<type>Geocache|Project APE Cache</type>
<extensions>
 <groundspeak:cache>
    <groundspeak:name><![CDATA[#{vrnazev}]]></groundspeak:name>
    <groundspeak:placed_by>-</groundspeak:placed_by>
    <groundspeak:container>Other</groundspeak:container>
 </groundspeak:cache>

 <gpxg:GeogetExtension>
   <gpxg:Tags>
     <gpxg:Tag Category="poradi">#{poradi}</gpxg:Tag>
     <gpxg:Tag Category="vzdalenost">#{vzdalenost}</gpxg:Tag>
     <gpxg:Tag Category="prominence">#{prominence}</gpxg:Tag>
     <gpxg:Tag Category="elevationVr">#{vrele}</gpxg:Tag>
     <gpxg:Tag Category="elevationKs">#{ksele}</gpxg:Tag>
     <gpxg:Tag Category="elevationMv">#{mvele}</gpxg:Tag>
   </gpxg:Tags>
 </gpxg:GeogetExtension>
</extensions>
</wpt>

<wpt lat="#{kslat}" lon="#{kslon}">
<time>#{cas}</time>
<name>KSV#{vridentif}</name>

<desc><![CDATA[#{ksnazev} Klíčové sedlo #{ksele} m.n.m]]></desc>
<type>Waypoint|Reference Point</type>
<extensions>
 <gpxg:GeogetExtension>
   <gpxg:Tags>
     <gpxg:Tag Category="elevation2">#{ksele}</gpxg:Tag>
   </gpxg:Tags>
 </gpxg:GeogetExtension>
</extensions>
</wpt>

<wpt lat="#{mvlat}" lon="#{mvlon}">
<time>#{cas}</time>
<name>MVV#{vridentif}</name>
<desc><![CDATA[#{mvnazev} Mateřský vrchol #{mvele} m.n.m]]></desc>
<type>Waypoint|Reference Point</type>
<extensions>
 <gpxg:GeogetExtension>
   <gpxg:Tags>
     <gpxg:Tag Category="elevation2">#{mvele}</gpxg:Tag>
   </gpxg:Tags>
 </gpxg:GeogetExtension>
</extensions>
</wpt>
|]


-- 49.2839519N, 16.3563408E
bodyXml :: (String -> Maybe String) -> [Vrch] -> String
bodyXml nazevFce vrchyp = 
                let 
                    sezSeVzdalenosti :: [(Integer, GpsVrch)]
                    sezSeVzdalenosti = map (\ gpp@(GpsVrch _ (GpsKopec sou _ _) _ _) -> (round (distance2 hcGps sou), gpp)) 
                      . map vrch2gps $ vrchyp
                    
                    sezSortedDleVysek  :: [(Integer, GpsVrch)] 
                    sezSortedDleVysek =  sezSeVzdalenosti -- reverse $ sortBy (compare `on` dejVysku) sezSeVzdalenosti
                    (vzdalenosti, vrchy) = unzip sezSortedDleVysek
                    sez2 = zip (zip [1..] vzdalenosti) vrchy -- řazení: výšky, obsahuje vzdálenost i pořadí dle výšek
                    sez3 = sortBy (compare `on` (snd.fst)) sez2 -- řazení: vzdálenosti

                    sez :: [((Int, Integer), GpsVrch)] 
                    sez = let -- rozbalit a zabalit, aby se udelali vzdalenosti unikatni
                            (poradiAVzdalenosti2, vrchy2) = unzip sez3
                            (poradi2, vzdalenosti2) = unzip poradiAVzdalenosti2
                          in zip (zip poradi2 (rostouci vzdalenosti2)) vrchy2 -- opraveny seznam, aby vzdalenosti byly unikatni

                    vrchyJakoXmlStr :: [String]
                    vrchyJakoXmlStr = map (bodXml nazevFce)  sez
                          
                in hlavicka ++ (concat (vrchyJakoXmlStr)) ++ paticka           
        where dejVysku (_, (GpsVrch _ (GpsKopec _ mnm _) _ _  )) = mnm

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (lat1 , lon1) (lat2 , lon2) = sqrt (lat'*lat' + lon'*lon')
    where lat' = (lat1 - lat2) * 60 * 1852
          lon' = (lon1 - lon2) * 60 * 1852 * (lat1 / 180 * pi)
        