--
-- Celistvý binární strom má každý uzel buď dva potomky nebo je to list. Jiná možnost není.
-- Neexistuje tedy prázdný strom
--
-- Dovojseznam je seznam dvou obecně různých typů prvků. Motivace je příbojová vlna ve 
-- Fortunově algoritmu, ale lze použít i pro jiné účely. Podle toho si seznam vysvětlíme:
-- Typy prvků jsou:
--   a .... křivky 
--   u .... průsečíky těchto křivek
-- Důležité je, že se prvky střídají a seznam začíná a končí křivkou. Neexistuje prázdný seznam.
--        a u a u ... u a
--
-- Implementace je celistvým nevyváženým binárním stromem. Navenek by však tato 
-- implemetnace neměla být viditelná a koncepčně musíme uvažovat v intencích seznamu,.
--
-- Seznam se prohledává vždy binárně, tak že jke zadaná funkce, která dostane průsečík a určuje, zda se pohybovat doleva nebo doprava.
-- Vlkládání a mazání má O(log), pokud je strom vyvážený. 
--
--
module Dvojseznam (
   Dlist,   -- to je ten seznam
   delete, -- vymaže ze seznamu křivku i s jejími dvěma okolními průsečíky a ty nahradí zadaným průsečíkem
   replace, -- nahradí křivku podseznamem
   find,   --- najde jedinou křivku
   find3, --   najde křivku s oklolními dvěma průsečíky
   find7,   -- najde křivku s okolím, tedy celkem tři křivky a čtyři průsečíky.


   singleton, -- vytvoří seznam s jedinou křivkou
   make3, -- vytvoří seznam se dvěma křivkami a průsečíkem
   make5, -- vytvoří seznam se třemi křivkami a dvěma prosečíky

   compose, -- spojí dva seznamy a průsečíkem

   Smer(..), -- Výsledek prohledávací funkce
   -- A nějaký ladící bordel
   rod,

   
   pp,
   ppf,
   jmena,
   fromListPolociselny,
   
) where

import Data.Maybe
import Data.Tuple
import  Control.Arrow

data Dlist a u = Leaf a | Node (Dlist a u) u (Dlist a u)
  deriving Show

data Smer = Levy | Pravy
  deriving Show

type Okoli a u =  (Maybe u, a, Maybe u)

singleton :: a -> Dlist a u
singleton x = Leaf x

-- Tady by mělo být případné vyvažování stromu
compose :: Dlist a u -> u -> Dlist a u -> Dlist a u
compose = Node

make3 :: a -> u -> a -> Dlist a u
make3 x1 u x2 = compose (singleton x1) u (singleton x2)

make5 :: a -> u -> a -> u -> a -> Dlist a u
make5 x1 u12 x2 u23 x3 = compose (make3 x1 u12 x2) u23 (singleton x3)


smeruj :: Smer -> (Dlist a u) -> (Dlist a u)
smeruj Levy (Node x _ _) = x
smeruj Pravy (Node _ _ x) = x

find :: (u -> Smer) -> (Dlist a u) -> a
find _ (Leaf x) = x
find f node@(Node _ u _) = find f (smeruj (f u) node)

find3 :: (u -> Smer) -> Dlist a u -> Okoli a u
find3 fce node = hledej (Nothing, Nothing) node
   where
    -- hledej ::  (Maybe u, Maybe u) -> (Dlist a u) -> (Maybe u, a, Maybe u)
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
replace :: (u -> Smer) -> (Okoli a u -> Dlist a u) -> Dlist a u -> (Okoli a u, Dlist a u, Dlist a u)
replace fceFind fceReplace = hledej (Nothing, Nothing) 
   where
    -- hledej ::  (Maybe u, Maybe u) -> (Dlist a u) -> (Okoli a u, Dlist a u, Dlist a u)
    hledej (lev, prav) (Leaf x) = let  okoli = (lev, x, prav)
                                       nahradnik = fceReplace okoli
                                  in (okoli, nahradnik, nahradnik)

    hledej (lev, prav) node@(Node lnode u rnode) =
          case (fceFind u) of
            Levy ->  let (okoli, nahradnik, novyUzel) = hledej (lev, Just u)  lnode
                     in  (okoli, nahradnik, Node novyUzel u rnode)
            Pravy -> let (okoli, nahradnik, novyUzel) = hledej (Just u, prav) rnode
                     in  (okoli, nahradnik, Node lnode u novyUzel)

-- Najde zadaný prvek i s okolím.
find7 :: (u -> Smer) -> Dlist a u -> (Maybe u, Maybe a, Maybe u, a, Maybe u, Maybe a, Maybe u)
find7 fceFind = hledej ((Nothing, Nothing), (Nothing, Nothing))
   where
    -- hledej ::  ((Maybe u, Maybe (Dlist a u)), (Maybe (Dlist a u), Maybe u)) -> (Dlist a u) -> (Maybe u, Maybe a, Maybe u, a, Maybe u, Maybe a, Maybe u)
    hledej (lduo@(_, lt1), rduo@(rt1, _)) (Leaf x) = let (ra, ru2) = dohledejPravy rduo
                                                         (lu2, la) = dohledejLevy lduo
                                                     in (lu2, la,   getU <$> lt1,   x,   getU <$> rt1,   ra, ru2)
                                                    --in (Nothing, Nothing,  Nothing,   x,   Nothing,   Nothing, Nothing)
    hledej (lduo@(_, qqqlt1), rduo@(rt1, _)) node@(Node lnode u rnode) =
            case (fceFind u) of
                Levy  -> hledej (lduo, (Just node, getU <$> rt1))  lnode
                Pravy -> hledej ((getU <$> qqqlt1, Just node), rduo)  rnode

    dohledejPravy ::  (Maybe (Dlist a u), Maybe u) -> (Maybe a, Maybe u)
    dohledejPravy (Nothing, _) = (Nothing, Nothing)
    dohledejPravy (Just (Node _ _ (Leaf x)), ru2) = (Just x, ru2)
    dohledejPravy (Just (Node _ _  nodik), ru2) = first Just $ nejlevejsi2 nodik

    dohledejLevy ::  (Maybe u, Maybe (Dlist a u)) -> (Maybe u, Maybe a)
    dohledejLevy (_, Nothing) = (Nothing, Nothing)
    dohledejLevy (lu2, Just (Node (Leaf x) _ _ )) = (lu2, Just x)
    dohledejLevy (lu2, Just (Node nodik _ _ )) = second Just $ nejpravejsi2 nodik

    
nejlevejsi2 :: (Dlist a u) -> (a, Maybe u)
nejlevejsi2 (Node (Leaf x) u _ ) = (x, Just u)
nejlevejsi2 (Node node _ _ ) = nejlevejsi2 node
nejlevejsi2 (Leaf x)  = (x, Nothing)

nejpravejsi2 :: (Dlist a u) -> (Maybe u, a)
nejpravejsi2 (Node _ u (Leaf x)) = (Just u, x)
nejpravejsi2 (Node _ _ node) = nejpravejsi2 node
nejpravejsi2 (Leaf x)  = (Nothing, x)

-- Vymaže jeden prvek (parabolu) i jeho okolními průsečíky a nahradí novým růsečíkem.
delete :: (u -> Smer) -> u -> Dlist a u -> Dlist a u
delete fceFind nahrada = fst . mazani
   where
     -- mazani :: Dlist a u -> (Dlist a u, Maybe Smer)
     mazani node@(Node lsub uzel rsub) =
              case (fceFind uzel) of
                Levy -> case lsub of 
                          Leaf _ -> (rsub, Just Levy)
                          _ -> let (n, nahradit) = mazani lsub
                                   (novyUzel, novaNahradit) = nahrazovani Levy nahradit 
                               in  (Node n uzel rsub, nahradit)
                Pravy -> case rsub of 
                          Leaf _ -> (lsub, Just Pravy)
                          _ -> let (n, nahradit) = mazani rsub
                                   (novyUzel, novaNahradit) = nahrazovani Pravy nahradit
                               in (Node lsub novyUzel n, novaNahradit) 
            where        
              -- nahrazovani  :: Smer -> Maybe Smer -> (u, Maybe Smer)  
              nahrazovani _ Nothing  = (uzel, Nothing)
              nahrazovani Levy (Just Levy)  = (uzel, Just Levy)
              nahrazovani Pravy (Just Pravy)  = (uzel, Just Pravy)
              nahrazovani _ _  = (nahrada, Nothing)


                              
getU :: Dlist a u -> u
getU (Node _ u _) = u
getU _ = error "Funkci getU nelze volat na Laef."

-- findKrajni :: Smer -> (Dlist a u) -> a
-- findKrajni _ (Leaf x) = x
-- findKrajni smer node = findKrajni smer (smeruj smer node)

-- findVlevo :: (u -> Smer) -> (Dlist a u)  -> Maybe a
-- findVlevo _ (Leaf _) = Nothing
-- findVlevo f (Node levy u pravy) =
--     case f u of
--         Levy -> findVlevo f levy
--         Pravy ->  let hledanecNapravo = findVlevo f pravy
--                   in if isJust hledanecNapravo then hledanecNapravo
--                                                else Just (findKrajni Pravy levy)
--------------------------------------------------------------------------------------------------------------------------------------
-- Pomocné a ladící.
--
pp :: Bool -> Smer
pp False = Levy
pp True = Pravy

ppf :: (a -> Bool) -> (a -> Smer)
ppf f x = pp (f x)

rod = Node  
    (Node (Leaf "martin") 1969 (Leaf "helena"))
       1983
    (Node (Leaf "vojta") 1997 (Leaf "radek"))
  

jmena = ["alice", "bob", "cyril", "dana", "emil", "franta", "gabina", "honza", "ivan", "jana", "karel", "lenka", "martin", "nora", "ondra", "pavel", "quido", "radel", "standa", "tonda", "uršula", "vojta", "waltr", "xaver", "yvona", "zuzana"]

fromListPolociselny :: [a] -> Dlist a Double
fromListPolociselny = makej 0
  where
     makej :: Int -> [a] -> Dlist a Double
     makej _ [] = error "Neumime prazdny strom"
     makej _ [x] = Leaf x
     makej delic sez =   -- Víme, že jsou nejméně dva
            let pulka = (length sez) `div` 2
                (sez1, sez2) = splitAt pulka sez
            in  Node (makej delic sez1)  (fromIntegral (delic + pulka) + 0.5) (makej (delic + pulka) sez2)
