{-# LANGUAGE NamedFieldPuns #-}

module Voronai
    ( 
        voronai,
        Voronai(..)
    ) where


import Control.Monad
import Control.Monad.State

import  qualified PrioritniFronta as Q

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
      