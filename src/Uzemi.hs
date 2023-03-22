--
-- Modul řeší vše, co se týká území a práce s ním, tedy pouze polohové pole.
-- Nepočítá s nadmořskýma výškama, vrcholy ani nic podobným.
-- 
-- Souřadnice bodu po třech vteřinách (0,0) odpovídá N0 E0
module Uzemi
    (  Mou(..), Sit0, Moustrov(..),
      (-:++), okoliMou, rozdelNaOstrovy, vystredMou, addMou, xx, yy, okoli0,
      nejkratsiSpoj, okoliMouSeMnou
    ) where

import qualified Data.Map.Lazy as M 
import qualified Data.Set as S
import Data.Maybe


data Mou = Mou Int Int
  deriving (Eq, Ord, Show, Read)

xx :: Mou -> Int  
xx (Mou x _) = x

yy :: Mou -> Int  
yy (Mou _ y) = y

type Sit0 = M.Map Mou    

newtype Moustrov = Moustrov [Mou]
  deriving (Eq, Ord, Show, Read)

-- Z nějaké sítě a daného bodu vyvere okolí tohoto bodu (bez něj)
-- ale jen ty, které jsou v dané síti 
okoli0 :: Sit0 a -> Mou -> [a]
okoli0 sit bod = catMaybes $ map dej0 (okoliMou bod) 
  where
    dej0 mou = M.lookup mou sit

okoliMouSeMnou :: Mou -> [Mou]
okoliMouSeMnou mou = (mou : okoliMou mou)


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

-- fronta se myslí analogie bojová nebo meteorologická, zde frona stejných vzdáleností
-- v klíčích jso učelní body fronty, v datech je množina bodů přes které lze projít s danou vzdáleností
type Fronta = M.Map Mou (S.Set Mou) 

nejkratsiSpoj :: [Mou] -> [Mou] -> [Mou] -> Moustrov
nejkratsiSpoj ostrov1 pobrezi ostrov2 = 
   let cil = S.fromList ostrov2  
       pobreziSCilem = cil `S.union` (S.fromList pobrezi) -- pri pruchodu polem nebude zajimave, co je cil a so pobrezi

       nej :: S.Set Mou -> Fronta -> S.Set Mou
       nej zbyvajici fronta    -- fronta se myslí analogie bojová nebo meteorologická, zde frona stejných vzdáleností
          | M.null fronta = S.empty -- máme nespojité oblasti, nelze se tam dostat
          | not $ M.keysSet fronta `S.disjoint` cil = -- pronikli jsme k cíli
              foldl S.union S.empty . M.elems $ M.restrictKeys fronta cil -- tak co jsem posbíral vracím, ty z cíle ne
          | otherwise = --   
                let novaFronta = posunFrontu zbyvajici fronta
                    celoFronty = M.keysSet novaFronta
                in nej (zbyvajici S.\\ celoFronty) (novaFronta)
   in  Moustrov . S.toList $
         nej pobreziSCilem (M.fromList $ zip ostrov1 (repeat S.empty))
            S.\\ (S.fromList ostrov1) -- odečteme startovní body, kterése tam propadly


-- posuneme se o jeden bod z celou frontou ale jen v ramci zadaného volného pole
posunFrontu :: S.Set Mou -> Fronta -> Fronta 
posunFrontu volnePole fronta = M.fromListWith S.union $ 
          M.toList fronta >>= (\ (mou, set) ->
            zip (filter (flip S.member volnePole) (okoliMou mou)) (repeat (mou `S.insert` set)) 
          )

