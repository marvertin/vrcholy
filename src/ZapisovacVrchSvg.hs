{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ZapisovacVrchSvg
    (   vrchSvg
    ) where
 
import Lib
import Uzemi
import Gps
import Control.Arrow
import VrchTypy(Vrch(..), Kopec(..))
import HraniceCeska
import Data.String.Interpolate ( i )
import Text.Printf (printf)
import Data.List
import Data.Function
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Colour
import Graphics.Svg
import Graphics.Svg.Core
import Numeric (showHex, showIntAtBase)
import Text.Printf (printf)
import Data.String.Interpolate ( i )

-- Bod v mapových souřadnicích, který se má vykreslit na sřed okna
xMapCenter = 18600   :: Double  
yMapCenter = 59600  :: Double

--Šířka na papě, která má být vykreslena do okna
xMapSize = 8400  :: Double
yMapSize = 5500  :: Double

-- Veliksot okna asi v pixelech (viewBox)
xWinSize = 1680   :: Double 
yWinSize = xWinSize * yMapSize / xMapSize

-- Zkreslení způsobené tím, že v zeměpisné délce jsou stupně kratší než v šířce
-- Není dobré toto použít, jak je použito, neboť to zkresluje i vlastní kreslené tvary, z kružnic dělá elipsy. Na souřadnice budeme muset jít jinak.

wgsZkresleni = 1 / cos (stredniSirkaCeska / 180 * pi) 

-- Poloměr kruhu jednoho bodu
polomerBodu = 5

------------------------------------------------------------------------------
-- Hlevní funkce vykreslující vše

vrchSvg :: [Vrch] -> String
vrchSvg vrchy = (unpack . toStrict . renderText)  $ svg (
     coord3vteriny (obrysRepubliky <> brenskyObdelnik <> svgPoints (map vrVrchol vrchy) <> domácíBod)
     <> proužek <> rámeček
  )

-- Vykreslení celého swg, především obálky. Parametrem je obsah, který se má vykresluit.
svg :: Element -> Element
svg content =
     doctype
     <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- txt xWinSize, Height_ <<- txt yWinSize, ViewBox_ <<- [i|0 0 #{xWinSize} #{yWinSize}|]]


-- Vlastní výpočty bodů

svgPoint :: Kopec -> Element
svgPoint (Kopec mnm (Moustrov (Mou x y : _))) = circle_ [Cx_ <<- txt x, Cy_ <<- txt y, R_ <<- txt polomerBodu, Fill_ <<-  škála r]
  where r =  (max 0 . min 1350) (mnm - 200)

svgPoints :: [Kopec] -> Element
svgPoints = mconcat . map svgPoint 


-------------------------------------------------------------------
-- Další objekty na mapě

-- Obrys české republiky
obrysRepubliky :: Element
obrysRepubliky = wgs84 $ path_ [ D_ <<- (hlava hraniceCeska) <>  (mconcat $ map usek (tail hraniceCeska) ) <> z,
                                           Stroke_ <<- "black", Stroke_width_ <<- "0.01"
                                         , Fill_ <<- "#EEEEEE"  ]
   where 
      usek (x, y) = lA x y
      hlava ((x, y): _) = mA x y


-- Obdelník N49° E16°, tam je i Brno i maršov.
brenskyObdelnik :: Element
brenskyObdelnik = rect_ [X_ <<- txt (16 * 1200), Y_ <<- txt (49 * 1200), Height_ <<- "1200", Width_ <<- "1200", Style_ <<- "fill:none;stroke-width:3;stroke:rgb(0,0,0)" ]

-- Domácí bod
domácíBod :: Element
domácíBod = 
    let k = 0.03
    in wgs84 $ path_ [ D_ <<- (mA (snd hcGps) (fst hcGps) <> mR (-k) 0 <> hR (2*k) <> mR (-k) (-k) <> vR (2*k)), Stroke_ <<- "black", Stroke_width_ <<- "0.005"  ]

----------------------------------------------------------------
-- Změny souřadnic

-- Transformace souřadnic tak, aby se to hezky vykreslilo podle zadaných parametrů nahoře programu
-- Kreslí v třívteřinových souřadnicích, počátek je průnik rovníku a nultého poledníku.
coord3vteriny :: Element -> Element
coord3vteriny = g_ [ Transform_ <<- [i|translate(#{ xWinSize / 2 },#{ yWinSize / 2}) scale(#{ xWinSize / xMapSize  }, #{- yWinSize / yMapSize * wgsZkresleni }) translate(#{ -xMapCenter },#{- yMapCenter })|] ] 
-- coord3vteriny = g_ [ Transform_ <<- "translate(0," <> txt (yOkna + vyskaOkna / 2) <> ") scale(1,-1) translate(0," <> txt (-yOkna - vyskaOkna / 2) <> ")" ] 

-- Umožnít kreslit ve Wgs souřadnicích, ale musí být uvnitř coord3vteriny
wgs84 = g_ [ Transform_ <<- "scale(1200, 1200)" ] 

----------------------------------------------------------------
-- Pomocné objekty v rámci základního souřadnicového systému (mimo mapu)

-- Rámeček kolem všeho, ať vidíme, co zorbazí prohlížeč.
rámeček = g_ [Style_ <<- "fill:none;stroke-width:3;stroke:rgb(0,0,0)"] $ rect_ [X_ <<- txt 1, Y_ <<- txt 1, Width_ <<- txt (xWinSize - 1), Height_ <<- txt (yWinSize - 1) ]

-- Dle zadané hodnoty 0 ... 1535 vybere bervu na nějkaké škále a vyhádří v HTML podobě
škála :: Int -> Text
škála n = colorToHex (barvi (n + 1024)) (barvi n) (barvi (n + 512))
  where
     barvi n =  case ((n `mod` 1536) `div` 256) of
         0 -> n `mod` 256
         1 -> 255
         2 -> 255
         3 -> 255 - n `mod` 256
         4 -> 0
         5 -> 0

-- Barevný proužek hodnot výšek
proužek :: Element
proužek = 
  let
     proužeček n = rect_ [X_ <<- txt (n), Y_ <<- txt (0), Height_ <<- "30", Width_ <<- "1", škála n ->> Fill_]
  in mconcat $ map proužeček [0..1350]


--------------------------------------------------------------
---- Pomocné funkce

-- Převedení na text
txt :: Show a => a -> Text 
txt = pack . show 

colorToHex :: Int -> Int -> Int -> Text
colorToHex r g b = pack $ printf "#%02x%02x%02x" r g b

-- Asi hranice oblasti, kde se počítaly vrcholy, nepoužívá se.

hranice :: [Mou] -> (Mou, Mou)
hranice mous =
   let xxs = map xx mous
       yys = map yy mous
   in (  Mou (foldr1 min xxs) (foldr1 min yys), Mou (foldr1 max xxs) (foldr1 max yys)  )  



