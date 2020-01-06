module Hledac
    ( 
    zamapuj,
    jeKandidat,
    jeProminentni,
    rozdelNaOstrovy,
    odstranDuplicity,
    ) where

import Lib
import Data.List
import Data.Maybe

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

zamapuj :: [Bod] -> Sit
zamapuj = M.fromList . (map (\b -> (fst b, snd b)))

minimalniProminence = 10

okoli0 :: Sit0 a -> Mou -> [a]
okoli0 sit bod = catMaybes $ map dej0 (okoliMou bod) 
  where
    dej0 mou = M.lookup mou sit
    

-- Okolí nějak=ého bodu bez tohoto budu, tedy 8 okolních bodů
okoliMou :: Mou -> [Mou]
okoliMou (x,y) = [
    (x-1, y-1),
    (x-1, y),
    (x-1, y+1),
    (x, y-1),
    (x, y+1),
    (x+1, y-1),
    (x+1, y),
    (x+1, y+1)
  ]
        
    
okoli :: Sit -> Mou -> [Bod]
okoli sit bod = 
    map (dej sit) (okoliMou bod) 



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
           Just udaj -> ost (M.delete m sit) (okoliMou m ++ mrest) (M.insert m udaj o : orest)


odstranDuplicity :: [Bod] -> [Bod]           
odstranDuplicity body = map (vystred . M.assocs) ((rozdelNaOstrovy . zamapuj) body)


-- vyrobí bod z prázdného seznamu, který je nějako uprostřed        
vystred :: [Bod] -> Bod
vystred body =
    let n = length body
    in ((
         (sum $ map (fst . fst) body) `div` n,
         (sum $ map (snd . fst) body) `div` n
       ), (snd . head) body)



jeKandidat :: Sit -> Bod -> Bool
jeKandidat sit (mou, vyska) =
    all (\(_,v) -> v <= vyska) (okoli sit mou)

jeProminentni :: Sit -> Bod -> Bool
jeProminentni sit bod@(_, vyska) = promi S.empty [bod]
   where 
       promi :: S.Set Mou -> [Bod] -> Bool
       promi _ [] = True -- vse provereno, kopec je tedy prominentni
       promi kopec ((m, v) : rest)  
           | S.member m kopec = promi kopec rest -- když už jsme prověřovali, nemusíme znovu a je prominentní
           | v > vyska = False -- narazili jsme na vyšší bod než kopec přes nízké sedlo, takže kopec není prominentní
           | vyska - v > minimalniProminence = promi kopec rest -- narazili jsme na bod pod prominencí, tak okolí neprověřujeme, jen zbylé body
           | otherwise = promi (S.insert m kopec) (okoli sit m ++ rest)

dej :: Sit -> Mou -> Bod
dej sit mou =
    case (M.lookup mou sit) of
       Nothing -> (mou, 10000) -- hodne moc je kolem nas
       Just mnm -> (mou, mnm)
      

