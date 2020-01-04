module Hledac
    ( 
    okoli,
    zamapuj,
    jeKandidat
    ) where

import Lib
import Data.List

import qualified Data.Map.Lazy as M

zamapuj :: [Bod] -> Sit
zamapuj = M.fromList . (map (\b -> (fst b, b)))

okoli :: Sit -> Bod -> [Bod]
okoli sit ((x,y),_) = [
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
jeKandidat sit bod@(_,vyska) =
    all (\(_,v) -> v <= vyska) (okoli sit bod)

dej :: Sit -> Mou -> Bod
dej sit mou =
    case (M.lookup mou sit) of
       Nothing -> (mou, 10000) -- hodne moc je kolem nas
       Just bod -> bod 
      