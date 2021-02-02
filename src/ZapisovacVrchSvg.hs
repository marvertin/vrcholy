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

txt :: Show a => a -> Text 
txt = pack . show 

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "2100", Height_ <<- "1200", ViewBox_ <<- "15600 0 8400 4800"]


logo :: Element
logo =
     path_ [ Fill_ <<- "#352950"
           , D_ <<- ( mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0
                   <> lA 198 170 <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 ) ]
  <> path_ [ Fill_ <<- "#4A3A74"
           , D_ <<- ( mA 113 340 <> lA 226 170 <> lA 113 0 <> lA 198 0
                   <> lA 425 340 <> lA 340 340 <> lA 269 234 <> lA 198 340
                   <> lA 113 340 <> z <> mA 113 340 ) ]
  <> path_ [ Fill_ <<- "#7C3679"
           , D_ <<- ( mA 387 241 <> lA 350 184 <> lA 482 184 <> lA 482 241
                   <> lA 387 241 <> z <> mA 387 241 ) ]
  <> path_ [ Fill_ <<- "#7C3679"
           , D_ <<- ( mA 331 156 <> lA 293 99 <> lA 482 99 <> lA 482 156
                   <> lA 331 156 <> z <> mA 331 156 ) ]
  <> circle_ [Cx_ <<- "150", Cy_ <<- "100", R_ <<- "80", Fill_ <<- "#33ccbb"]                   

colorToHex :: Int -> Int -> Int -> Text
colorToHex r g b = pack $ printf "#%02x%02x%02x" r g b

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

proužek :: Element
proužek = 
  let
     proužeček n = rect_ [X_ <<- txt (4 * n + 15600), Y_ <<- txt (57600), Height_ <<- "200", Width_ <<- "8", škála n ->> Fill_]
  in mconcat $ map proužeček [0..1350]

svgPoint :: Kopec -> Element
svgPoint (Kopec mnm (Moustrov (Mou x y : _))) = circle_ [Cx_ <<- txt x, Cy_ <<- txt (62400 - y), R_ <<- "5", Fill_ <<-  škála r]                   
  where r =  (max 0 . min 1350) (mnm - 200)

svgPoints :: [Kopec] -> Element
svgPoints = mconcat . map svgPoint 

-- extractMou :: Vrch -> Kopec 
-- extractMou Vrch { vrVrchol = Kopec _ (Moustrov (mou:_)) } = mou

hranice :: [Mou] -> (Mou, Mou)
hranice mous =
   let xxs = map xx mous
       yys = map yy mous
   in (  Mou (foldr1 min xxs) (foldr1 min yys), Mou (foldr1 max xxs) (foldr1 max yys)  )  

-- 49.2839519N, 16.3563408E
vrchSvg :: [Vrch] -> String
vrchSvg vrchy = (unpack . toStrict . renderText)  $ svg (svgPoints (map vrVrchol vrchy) <> proužek)
-- vrchSvg vrchy = show $ hranice (map extractMou vrchy)
-- (Mou 15600 57600, Mou 23999 62399)  8400 x 4800