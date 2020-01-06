module Hledac
    ( 
    okoli,
    zamapuj,
    jeKandidat,
    jeProminentni,
    rozdelNaOstrovy,
    odstranDuplicity,
    ) where

import Lib
import Data.List

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

zamapuj :: [Bod] -> Sit
zamapuj = M.fromList . (map (\b -> (fst b, snd b)))

minimalniProminence = 10

okoli :: Sit -> Mou -> [Bod]
okoli sit (x,y) = [
    dej sit (x-1, y-1),
    dej sit (x-1, y),
    dej sit (x-1, y+1),
    dej sit (x, y-1),
    dej sit (x, y+1),
    dej sit (x+1, y-1),
    dej sit (x+1, y),
    dej sit (x+1, y+1)
  ]

-- Rozdělí hustou síť na ostrovy podle děr v neexistujících datech
rozdelNaOstrovy :: Sit -> [Sit]
rozdelNaOstrovy sit  = ost sit [] []
  where
     --     zbývající body -> pozice k probrání -> vyrobené ostrovy -> vsechny ostroy
     ost :: Sit -> [Mou] -> [Sit] -> [Sit]   --
     ost sit [] ovy 
       | M.null sit = ovy -- je to hotovo
       | otherwise = ost sit [ (fst . head . M.assocs) sit ] (M.empty : ovy) -- zahajujeme nový ostrov
     ost sit (m : mrest) oo@(o : orest)  
       | M.member m o = ost sit mrest oo -- už ho máme v ostrově
       | otherwise = case sit M.!? m of
           Nothing -> ost sit mrest oo -- bod odděluje ostrovy
           Just udaj -> ost (M.delete m sit) (map fst (okoli sit m) ++ mrest) (M.insert m udaj o : orest)


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
      