module Hledac
    ( 
    okoli,
    zamapuj,
    jeKandidat,
    jeProminentni
    ) where

import Lib
import Data.List

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

zamapuj :: [Bod] -> Sit
zamapuj = M.fromList . (map (\b -> (fst b, b)))

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

jeKandidat :: Sit -> Bod -> Bool
jeKandidat sit (mou ,vyska) =
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
       Just bod -> bod 
      