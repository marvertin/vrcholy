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
import VrchTypy(Vrch(..), Misto(..))
import HraniceCeska
import Cesko
import Data.String.Interpolate ( i )
import Text.Printf (printf)
import Data.List
import Data.Foldable
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
(xMapCenter, yMapCenter) = mouToKm (Mou 18600 59600)

--Šířka na papě, která má být vykreslena do okna
(xMapSize, yMapSize) = mouToKm (Mou 8400 5500)

-- Veliksot okna asi v pixelech (viewBox)
xWinSize = 1680   :: Double 
yWinSize = xWinSize * yMapSize / xMapSize


-- Poloměr kruhu jednoho bodu
polomerBodu = 0.5

------------------------------------------------------------------------------
-- Hlevní funkce vykreslující vše

vrchSvg :: [Vrch] -> String
vrchSvg vrchy = (unpack . toStrict . renderText)  $ svg (
     coord3vteriny (
                 
                spojniceDvouKopcus "yellow" (map (\x -> (vrVrchol x, vrKlicoveSedlo x)) vrchy) 
             <> spojniceDvouKopcus "gray" (map (\x -> (vrKlicoveSedlo x, vrMaterskyVrchol x)) vrchy) 
             <> brenskyObdelnik
             <> obrysRepubliky
             <> svgPoints (map vrVrchol vrchy) 
             <> spojniceDvouKopcus "black" (vyskovnice $ map vrVrchol vrchy) -- Výškovnice má neefektivní algortimus a je proředěna
             <> domácíBod
          ) <> proužek <> rámeček
  )

-- Vykreslení celého swg, především obálky. Parametrem je obsah, který se má vykresluit.
svg :: Element -> Element
svg content =
     doctype
     <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- txt xWinSize, Height_ <<- txt yWinSize, ViewBox_ <<- [i|0 0 #{xWinSize} #{yWinSize}|]]


-- Vlastní výpočty bodů

svgPoint :: Misto -> Element
svgPoint (Misto mnm mou) = circle_ [Cx_ <<- txt x, Cy_ <<- txt y, R_ <<- txt polomerBodu, Fill_ <<-  škála r]
  where r =  (max 0 . min 1350) (mnm - 200)
        (x, y) = mouToKm mou

svgPoints :: [Misto] -> Element
svgPoints = mconcat . map svgPoint 

spojniceDvouKopcu :: Text -> Misto -> Misto -> Element
spojniceDvouKopcu barva (Misto _ mou1) (Misto _ mou2)
             = line_ [X1_<<- txt x1, X2_ <<- txt x2, Y1_ <<- txt y1, Y2_ <<- txt y2, Stroke_ <<- barva, Stroke_width_ <<- "0.1"]
        where 
           (x1, y1) = mouToKm mou1
           (x2, y2) = mouToKm mou2

spojniceDvouKopcus ::  Text -> [(Misto, Misto)] -> Element
spojniceDvouKopcus barva = mconcat . map (uncurry (spojniceDvouKopcu barva))

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
brenskyObdelnik = wgs84 $ rect_ [X_ <<- txt 16,  Y_ <<- txt 49, Height_ <<- "1", Width_ <<- "1", Style_ <<- "fill:none;stroke-width:0.005;stroke:rgb(0,0,0)" ]

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
coord3vteriny = g_ [ Transform_ <<- [i|translate(#{ xWinSize / 2 },#{ yWinSize / 2}) scale(#{ xWinSize / xMapSize  }, #{- yWinSize / yMapSize  }) translate(#{ -xMapCenter },#{- yMapCenter })|] ] 
-- coord3vteriny = g_ [ Transform_ <<- "translate(0," <> txt (yOkna + vyskaOkna / 2) <> ") scale(1,-1) translate(0," <> txt (-yOkna - vyskaOkna / 2) <> ")" ] 

-- Umožnít kreslit ve Wgs souřadnicích, ale musí být uvnitř coord3vteriny
wgs84 = 
   g_ [ Transform_ <<- [i|scale(#{xScale}, #{yScale})|] ] 
   where (xScale, yScale) = mouToKm(Mou 1200 1200)
 

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


---------------------------------------------------
-- Pokusný výpočet výškovnice.
--
-- Pro každé místo, určí místo dvojici v níž je toto místo a pak nejbližší vyšší místo k tomuto místu.
-- Je zde použita neefaktivní kvadratická implementace !!!!

-- Proto si vrcholy ředíme. Sem dej maximální počet vrcholů, který zvládneš. KDyž dáš velé číslo, budeš mít všechny.
maximálníPočetMístProVýškovnicei = 100     -- Sem si dej jiné číslo, když chceš ladit. Je to kvůli výkonnosti


vyskovnice :: [Misto] -> [(Misto, Misto)]
vyskovnice vrchy = 
   let prořeďenéVrchy = prořeďMísta vrchy
   in  fmap (\vrch -> (vrch, nejblizsiVyssi prořeďenéVrchy vrch) )  prořeďenéVrchy

nejblizsiVyssi ::  [Misto] -> Misto -> Misto
nejblizsiVyssi vrchy mujvrch = 
   let jenVyssi = filter (jeMensi mujvrch) vrchy :: [Misto]
       porovnani = compare `on` dalkaKvadrat mujvrch :: Misto -> Misto -> Ordering
   in  if jenVyssi == [] then mujvrch 
                         else minimumBy porovnani (filter (/=mujvrch) jenVyssi)

jeMensi :: Misto -> Misto -> Bool
--jeMensi v1 v2 = vyska v1 < vyska v2
jeMensi = (<) `on` vyska

vyska :: Misto -> Int
vyska (Misto mnm _) =  mnm

dalkaKvadrat :: Misto -> Misto -> Int
dalkaKvadrat (Misto _ (Mou x1 y1)) (Misto _ (Mou x2 y2)) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

prořeďMísta :: [Misto] -> [Misto]
prořeďMísta mista = prořeď (length mista `div` maximálníPočetMístProVýškovnicei + 1) mista

-- Proředí seznam tak, že bere každou n-tou
prořeď :: Int -> [a] -> [a]
prořeď _ [] = []
prořeď n list
     | n < 0 = list
prořeď n list = (head list: prořeď n (drop n list))