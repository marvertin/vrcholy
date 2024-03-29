--
-- Modul pro přímé hledání vrchol (Momentálně nepoužíváno)
--
--   1. Vyhledají se kandidáti, což jsou vrcholy, které žádný okolní vrchol nepřevyšuje,
--       ale může mít kolem sebe stejně vysoké vrcholy
--   2. Rozhlížíme se kolem sebe dokud:
--        buď nedorazíme na másto vyšší než náš kadidát, pak kandidát není vrcholem
--        nebo ve všech směrech narazíme na bod nižší, než je daná prominence, pak máme vrchol   
module Hledac
    ( 
    zamapuj, -- kvuli ladeni
    najdiVrcholyZBodu
    ) where

import Lib
import Uzemi
import VrchTypy

import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

zamapuj :: [Bod] -> Sit
zamapuj = M.fromList . (map (\b -> (fst b, snd b)))

minimalniProminence = 10

najdiVrcholyZBodu :: Mnm -> [Bod] -> [Vrch]
najdiVrcholyZBodu minimalniPromnence body =
    let sit = zamapuj body
        kandidati = filter (jeKandidat sit) body
        prominentniVrcholy = filter (jeProminentni sit) kandidati
        kopce = stvorKopce prominentniVrcholy
    in map (\kopec -> Vrch {vrVrchol = kopec, 
        vrKlicoveSedlo = Kopec 0 (Moustrov []),
        vrMaterskeVrcholy = Kopec 0 (Moustrov []) } ) kopce
  where
    stvorKopce :: [Bod] -> [Kopec]
    stvorKopce body = map kopecZeSite ((rozdelNaOstrovy . zamapuj) body)

    kopecZeSite :: Sit -> Kopec
    kopecZeSite sit = Kopec ((snd . M.findMin) sit) ((Moustrov . M.keys) sit)

-- vyrobí bod z prázdného seznamu, který je nějako uprostřed        
vystred :: [Bod] -> Bod
vystred body = (vystredMou (map fst body), (snd . head) body)

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

okoli :: Sit -> Mou -> [Bod]
okoli sit bod = 
    map (dej sit) (okoliMou bod)        
      
