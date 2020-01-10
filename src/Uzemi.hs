--
-- Modul řeší vše, co se týká území a práce s ním, tedy pouze polohové pole.
-- Nepočítá s nadmořskýma výškama, vrcholy ani nic podobným.
-- 
-- Souřadnice bodu po třech vteřinách (0,0) odpovídá N0 E0
module Uzemi
    (  Mou(..), Sit0, Moustrov(..),
      (-:++), okoliMou, rozdelNaOstrovy, vystredMou, addMou, xx, yy,
      nejkratsiSpoj
    ) where

import qualified Data.Map.Lazy as M 
import Data.Maybe


data Mou = Mou Int Int
  deriving (Eq, Ord, Show, Read)

xx :: Mou -> Int  
xx (Mou x _) = x

yy :: Mou -> Int  
yy (Mou _ y) = y

type Sit0 = M.Map Mou    

newtype Moustrov = Moustrov [Mou]
  deriving (Eq, Ord)

-- Z nějaké sítě a daného bodu vyvere okolí tohoto bodu (bez něj)
-- ale jen ty, které jsou dané síti 
okoli0 :: Sit0 a -> Mou -> [a]
okoli0 sit bod = catMaybes $ map dej0 (okoliMou bod) 
  where
    dej0 mou = M.lookup mou sit
    

-- Okolí nějak=ého bodu bez tohoto budu, tedy 8 okolních bodů
okoliMou :: Mou -> [Mou]
okoliMou mou = mou -:++ []

(-:++) :: Mou -> [Mou] -> [Mou]
Mou x y -:++ moul = 
  let xp = x + 1
      xm = x - 1
      yp = y + 1  
      ym = y - 1  
  in (
        Mou xm ym :
        Mou xm y :
        Mou xm yp :
        Mou x ym :
        Mou x yp :
        Mou xp ym :
        Mou xp y :
        Mou xp yp :
        moul
    )

addMou :: Int -> Int -> Mou -> Mou
addMou dX dY (Mou x y) = Mou (dX + x) (dY + y)


-- vyrobí bod z neprázdného seznamu, který je nějako uprostřed        
vystredMou :: [Mou] -> Mou
vystredMou mous =
    let n = length mous
    in Mou
         ((sum $ map xx mous) `div` n)
         ((sum $ map yy mous) `div` n)


  
  
-- Rozdělí hustou síť na ostrovy podle děr v neexistujících datech
-- je mu úplně jedno, co jsou hodnoty, zda výšky nebo něco jiného, to se bude hodit při počítání prominencí
rozdelNaOstrovy :: Sit0 a -> [Sit0 a]
rozdelNaOstrovy sit  = ost sit [] []
  where
     --     zbývající body -> pozice k probrání -> vyrobené ostrovy -> vsechny ostroy
     ost :: Sit0 a -> [Mou] -> [Sit0 a] -> [Sit0 a]   --
     ost sit [] ovy 
       | M.null sit = ovy -- je to hotovo
       | otherwise = ost sit [ (fst . head . M.assocs) sit ] (M.empty : ovy) -- zahajujeme nový ostrov
     ost sit (m : mrest) oo@(o : orest)  
       | M.member m o = ost sit mrest oo -- už ho máme v ostrově
       | otherwise = case sit M.!? m of
           Nothing -> ost sit mrest oo -- bod odděluje ostrovy
           Just udaj -> ost (M.delete m sit) (m -:++ mrest) (M.insert m udaj o : orest)


nejkratsiSpoj :: [Mou] -> [Mou] -> [Mou] -> Moustrov
nejkratsiSpoj ostrov1 pobrezi ostrov2 = Moustrov []