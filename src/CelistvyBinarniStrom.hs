--
-- Celistvý binární strom má každý uzel buď dva potomky nebo je to list. Jiná možnost není.
-- Neexistuje tedy prázdný strom
module CelistvyBinarniStrom (
   rod,
   findLeaf,
   findKrajni,
   findVlevo,
   findLeafOkoli,
   replace,

   singleton,
   make3,
   make5,
   Smer(..),
   pp,
   ppf
) where

import Data.Maybe

data Tree a u = Leaf a | Node (Tree a u) u (Tree a u)
  deriving Show

data Smer = Levy | Pravy
  deriving Show

type Okoli a u =  (Maybe u, a, Maybe u)

singleton :: a -> Tree a u
singleton x = Leaf x

-- Tady by mělo být případné vyvažování stromu
compose :: Tree a u -> u -> Tree a u -> Tree a u
compose = Node

make3 :: a -> u -> a -> Tree a u
make3 x1 u x2 = compose (singleton x1) u (singleton x2)

make5 :: a -> u -> a -> u -> a -> Tree a u
make5 x1 u12 x2 u23 x3 = compose (make3 x1 u12 x2) u23 (singleton x3)

pp :: Bool -> Smer
pp False = Levy
pp True = Pravy

ppf :: (a -> Bool) -> (a -> Smer)
ppf f x = pp (f x)

smeruj :: Smer -> (Tree a u) -> (Tree a u)
smeruj Levy (Node x _ _) = x
smeruj Pravy (Node _ _ x) = x

findLeaf :: (u -> Smer) -> (Tree a u) -> a
findLeaf _ (Leaf x) = x
findLeaf f node@(Node _ u _) = findLeaf f (smeruj (f u) node)

findLeafOkoli :: (u -> Smer) -> Tree a u -> Okoli a u
findLeafOkoli fce node = hledej (Nothing, Nothing) node
   where
    -- hledej ::  (Maybe u, Maybe u) -> (Tree a u) -> (Maybe u, a, Maybe u)
    hledej (lev, prav) (Leaf x) = (lev, x, prav)
    hledej (lev, prav) node@(Node lnode u rnode) =
        case (fce u) of
          Levy -> hledej (lev, Just u)  lnode
          Pravy -> hledej (Just u, prav) rnode

-- Výměna v seznamu.
-- 1. Hledací funkce
-- 2. Funkce výmeny po nalezení
-- 3. Strom, kde se hledá
-- Result: tuple
--      - to co se našlo i s okolím
--      - strom, za který to nalezené bylo vyměněno
--      - stropm po výměně          
replace :: (u -> Smer) -> (Okoli a u -> Tree a u) -> Tree a u -> (Okoli a u, Tree a u, Tree a u)
replace fceFind fceReplace = hledej (Nothing, Nothing) 
   where
    -- hledej ::  (Maybe u, Maybe u) -> (Tree a u) -> (Okoli a u, Tree a u, Tree a u)
    hledej (lev, prav) (Leaf x) = let  okoli = (lev, x, prav)
                                       nahradnik = fceReplace okoli
                                  in (okoli, nahradnik, nahradnik)

    hledej (lev, prav) node@(Node lnode u rnode) =
          case (fceFind u) of
            Levy ->  let (okoli, nahradnik, novyUzel) = hledej (lev, Just u)  lnode
                     in  (okoli, nahradnik, Node novyUzel u rnode)
            Pravy -> let (okoli, nahradnik, novyUzel) = hledej (Just u, prav) rnode
                     in  (okoli, nahradnik, Node lnode u novyUzel)



findKrajni :: Smer -> (Tree a u) -> a
findKrajni _ (Leaf x) = x
findKrajni smer node = findKrajni smer (smeruj smer node)

findVlevo :: (u -> Smer) -> (Tree a u)  -> Maybe a
findVlevo _ (Leaf _) = Nothing
findVlevo f (Node levy u pravy) =
    case f u of
        Levy -> findVlevo f levy
        Pravy ->  let hledanecNapravo = findVlevo f pravy
                  in if isJust hledanecNapravo then hledanecNapravo
                                               else Just (findKrajni Pravy levy)

rod = Node  
    (Node (Leaf "martin") 1969 (Leaf "helena"))
       1983
    (Node (Leaf "vojta") 1997 (Leaf "radek"))
  