{-# LANGUAGE NamedFieldPuns #-}

module Voronai
    ( 
        voronai,
        Voronai(..),
        prusecik,
        kontrola,
        kontrolaMox,
        sedi
    ) where


import Control.Monad
import Control.Monad.State

import  qualified PrioritniFronta as Q

import Data.Geometry.Point
import Data.Geometry.Triangle
import System.Random

import qualified Dvojseznam as D

-- Vstupní body trojúhelníka
p1 = Point2 1 1
p2 = Point2 5 5
p3 = Point2 9 1

-- Výpočet středu kružnice opsaného
-- circumcenter = triangleCircumcenter $ Triangle p1 p2 p3

data TypUdalosti = Mistni | Kruhova
   deriving (Show, Eq, Ord)

--                      y      typ      x  
data Udalost = Udalost Double TypUdalosti Double   
   deriving (Show, Eq, Ord)


-- Rozpracovaný voroného diagram 
data Voro = Voro { voKal :: Q.Prifron Udalost, vysl :: [String] }
   deriving (Show)

-- Výsledky zpracování voroného diagramu i s triangulací
data Voronai = Voronai [String]
   deriving (Show)


voronai :: [(Double, Double)] -> Voronai
voronai list = 
   let mistniUdalosti = Q.fromList $ map (\(x, y) -> Udalost y Mistni x) list
       uvodniVoro = Voro { voKal = mistniUdalosti, vysl = [] }
       Voro{vysl} = execState zpracujVse uvodniVoro
   in Voronai vysl    



type Stav = State Voro

-- Vrátí událost a přitom změní stav, kdy událost již není ve frontě.
-- Události jsou ztraceny
fetchUdalost :: Stav (Maybe Udalost)
fetchUdalost = do
    voro <- get
    case Q.maxView (voKal voro) of
          Just (x, voKal) -> do  
                             put voro{voKal}
                             return (Just x)
          Nothing -> return Nothing

zpracujVse :: Stav ()
zpracujVse = do
    ud <- fetchUdalost
    case ud of
      Just uda -> do 
                    zpracujUdalost uda
                    zpracujVse
      Nothing -> return ()


-- Tady se vše provádí, kdy se zpracovává jediná událost
zpracujUdalost :: Udalost -> Stav ()
zpracujUdalost ud = do
    voro@Voro{vysl} <- get
    put voro{vysl = vysl ++ [show ud] }
      

prusecik :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
prusecik (x1, y1) (x2, y2) yd
    | y1 == y2 =  ( (x1 + x2) / 2, (x1 + x2) / 2)
    | otherwise =
         let 
            m = (^2)
            yd1 = yd - y1
            yd2 = yd - y2
            a = yd2 - yd1
            b = -2 * (x1 * yd2 - x2 * yd1)
            c = (m x1 + m y1) * yd2 - (m x2 + m y2) * yd1 + (yd1 - yd2) * m yd
            d = m b - 4 * a * c
            sd = sqrt d
            xx1 = (-b + sd) / (2 * a)
            xx2 = (-b - sd) / (2 * a)
         in (xx1, xx2)   


kontrola :: (Double, Double) -> (Double, Double) -> Double -> ((Double, Double, Double), (Double, Double, Double))
kontrola b1@(x1, y1) b2@(x2, y2) yd =
     let
         m = (^2)
         dopoY xxx =  (m xxx - 2 * x1 * xxx + m x1 + m y1 - m yd) / (-2 * (yd - y1))
         (xa, xb) = prusecik b1 b2 yd
         ya = dopoY xa
         yb = dopoY xb
         d qx1 qy1 qx2 qy2 = sqrt $ (qx1 - qx2)^2 + (qy1 - qy2)^2
         vzdalenosti x0 y0 = 
            let dalka = d x0 y0
            in (dalka x1 y1, dalka x2 y2, dalka x0 yd)
     in (vzdalenosti xa ya,  vzdalenosti xb yb)  

notP :: (a -> Bool) -> (a -> Bool)
notP = (not .)

sedi :: ((Double, Double, Double), (Double, Double, Double)) -> Bool
sedi (a, b) =
  let 
     p === q = p - q < 0.0000001
     se (x, y, z) = x === y && y === z || isNaN x
  in se a && se b

paruj :: [a] -> [(a,a)]
paruj [] = []
paruj [_] = []
paruj (x : y : list) = ((x, y) : paruj list)

kontrolaMox = 
     let 
         cisla = randoms (mkStdGen 137) :: [Double]
         body = paruj $ (*100.1) <$> cisla
         dvojbody = take 200000 $ paruj body
     -- in take 20 dvojbody
     in filter (notP sedi . fst) $ zip ( (flip (uncurry kontrola) 13.5) <$> dvojbody ) dvojbody
     -- in (flip (uncurry kontrola) 13.5) <$> dvojbody

