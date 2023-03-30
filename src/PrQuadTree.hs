module PrQuadTree
    (   
        -- Toto zvěřejnit chceme
        Tree, -- Jen ten typ, žádné kosntruktory, není možné stvořit tree z venku
        emptyTree, pointToTree,  -- Jediný způsob jak stvořit nový strom
        unionTrees, intersectionTrees, differenceTrees, -- Běžné množinové operace
        
        -- Zveřejňujeme jen kvůli testování
        q1, q2, q3, q4, treeUp, qa, q5
    ) where

import Data.Ratio
import Debug.Trace

class  (Num a, Ord a) => Numd a  where
    -- | the rational equivalent of its real argument with full precision
    divd :: a -> a -> a
    floord :: Integral b =>  a -> b
    
    {-# INLINE div2 #-}
    div2 :: a -> a
    div2 = flip divd 2

instance Numd Int where
    divd = div
    floord = fromIntegral . id

instance Numd Integer where
    divd = div 
    floord = fromIntegral . id   

instance Numd Float where
    divd = (/) 
    floord = fromIntegral . floor

instance Numd Double where
    divd = (/)    
    floord = fromIntegral . floor

instance Integral a => Numd (Ratio a) where
    divd = (/)
    floord = fromIntegral . floor

-- Pořadí nódů je JZ, JV, SZ, SV
data Node i a =   EmpNd | Point (i, i) a | Node { jz :: (Node i a), jv :: (Node i a), sz :: (Node i a), sv :: (Node i a) }
   deriving Show
--                                JZ roh        velikost uzel
data Tree i a = Empty | Tree (Integer, Integer) Integer (Node i a)
   deriving Show
-- Obdélník je určen JZ a SV rohem. JZ do obélníku patří SV již nikoli. (Polouzavřené intervaly)
-- "Rect (5,8) (5,8)"" neobsahuje tedy žádný bod. JZ musí být menší nebo roven SV.

data Rect i = Rect (i,i) (i,i)
   deriving Show

-- Vlastní strom


instance (Numd i, Semigroup a, Show i, Show a) => Semigroup (Tree i a) where
   -- mempty = Empty
   (<>) = unionTrees

instance (Numd i, Semigroup a, Show i, Show a) => Monoid (Tree i a) where
   mempty = Empty

emptyTree :: Tree i a
emptyTree = Empty

-- Převede jedn bod i s vloženou hdonotou na strom, aby bylo možné se stromem
-- dále pracovat.
pointToTree :: Numd i => (i, i) -> a -> Tree i a
pointToTree xy@(x, y) dat = Tree (floord x, floord y) 1 (Point xy dat)

sameTreeSize :: Tree i a -> Tree i a -> (Tree i a, Tree i a)
sameTreeSize t1@(Tree xy1 size1 node1) t2@(Tree xy2 size2 node2)
   | size1 < size2 || xy1 > xy2 = sameTreeSize (treeUp t1) t2
   | size2 < size1 || xy2 > xy1 = sameTreeSize t1 (treeUp t2)
   | otherwise = (t1, t2)

-- Poposune nahoru ve čvercích celý strom, bdude tedy zabírat 2 * větší čtverec (čtyřikrát na plochu)
treeUp :: Tree i a -> Tree i a
treeUp Empty = Empty
treeUp (Tree (x, y) size node) =
    let size' = size * 2
    in 
          if y `mod` size' == 0 then if x `mod` size' == 0 then Tree (x,        y       ) size' (packNode $ Node {jz = node , jv = EmpNd, sz = EmpNd, sv = EmpNd})
                                                           else Tree (x - size, y       ) size' (packNode $ Node {jz = EmpNd, jv = node , sz = EmpNd, sv = EmpNd})
                                else if x `mod` size' == 0 then Tree (x,        y - size) size' (packNode $ Node {jz = EmpNd, jv = EmpNd, sz = node , sv = EmpNd})
                                                           else Tree (x - size, y - size) size' (packNode $ Node {jz = EmpNd, jv = EmpNd, sz = EmpNd, sv = node })

-- Ze stromu vytáhne úvodní obdélník. On je ve stromu dán JZ rohem a velikostí, my chceme rectangular se souřadnicemi.
treeRect :: Numd i => Tree i a -> Rect i
treeRect (Tree (x, y) size _) = 
    let x' = fromIntegral x
        y' = fromIntegral y
        size' = fromIntegral size  
    in Rect (x', y') (x' + size', y' + size')

unionTrees :: (Numd i, Semigroup a) => Tree i a -> Tree i a -> Tree i a
unionTrees =        mergeTrees id            id            (\xy a1 a2 -> Point xy (a1 <> a2))

intersectionTrees :: (Numd i) => Tree i a -> Tree i a -> Tree i a
intersectionTrees = mergeTrees (const EmpNd) (const EmpNd) (\xy a1 a2 -> Point xy a1)

differenceTrees :: (Numd i) => Tree i a -> Tree i a -> Tree i a
differenceTrees =   mergeTrees id            (const EmpNd) (const . const . const $ EmpNd)

mergeTrees :: (Numd i) => (Node i a -> Node i a) -> (Node i a -> Node i a) -> ((i,i) -> a -> a -> Node i a)
                  -> Tree i a -> Tree i a -> Tree i a
mergeTrees fceLeftNode fceRightNode fceSamePoints tree1 tree2 =
    let (tree1'@(Tree xy size node1), tree2'@(Tree _ _ node2)) = sameTreeSize tree1 tree2
    in Tree xy size (mergeNodes fceLeftNode fceRightNode fceSamePoints (treeRect tree1') node1 node2)

---------------------------------------------------------------------------------------------------------
-- Práce s nódy. To je vnitřek všeho.

unionNodes :: (Numd i, Semigroup a) => Rect i -> Node i a -> Node i a -> Node i a
unionNodes =        mergeNodes id            id            (\xy a1 a2 -> Point xy (a1 <> a2))

intersectionNodes :: (Numd i) => Rect i -> Node i a -> Node i a -> Node i a
intersectionNodes = mergeNodes (const EmpNd) (const EmpNd) (\xy a1 a2 -> Point xy a1)

differenceNodes :: (Numd i) => Rect i -> Node i a -> Node i a -> Node i a
differenceNodes =   mergeNodes id            (const EmpNd) (const . const . const $ EmpNd)

-- Toto je jádro celého Quad stromu. Umožňuje zmergovat rekurzivně dva uzly. Musí být na stejné úrovni čtverce.
-- Parametry mají význam:
--   1. Funkce, která se aplikuje ja osamocené levé uzly při budování výsledku.
--   2. Funkce, která se aplikuje na osamocené pravé uzly při budování výsledku.
--   3. Funkce, která se aplikuje na body stejných sopuřadnicích. Má za úkol sloučit data a vrací Node s temito daty, takže může vrátit EmpNd a tím odstraňovat.
--   4. Obdélník, kterého se vše týká (bude to čtverec)
--   5. První mergovaný nód
--   6. druhý mergovaný nód           
-- Výsledek je zmergovaný nód. Jak bude výsledek vypadat závisí na funkcíh. Může to být slednocení, průnik, rozdíl či úplně něco jiného.

mergeNodes :: (Numd i) => (Node i a -> Node i a) -> (Node i a -> Node i a) -> ((i,i) -> a -> a -> Node i a)
                 -> Rect i -> Node i a -> Node i a -> Node i a
mergeNodes fceLeftNode fceRightNode fceSamePoints = mergeNodes'
     where              
        mergeNodes' _ EmpNd node = fceRightNode node
        mergeNodes' _ node EmpNd = fceLeftNode node
        mergeNodes' _ (Point xy1 a1) (Point xy2 a2)  
            | xy1 == xy2 = fceSamePoints xy1 a1 a2
        mergeNodes' rect nodeOrPoint point@(Point _ _) = mergeNodes' rect nodeOrPoint (pointToNode rect point)
        mergeNodes' rect point@(Point _ _) node = mergeNodes' rect (pointToNode rect point) node
        mergeNodes' (Rect (x1,y1) (x2,y2)) (Node jz1 jv1 sz1 sv1) (Node jz2 jv2 sz2 sv2) = 
            packNode $ Node { 
                jz = mergeNodes' (Rect (x1, y1) (xmid, ymid)) (jz1) (jz2),
                jv = mergeNodes' (Rect (xmid, y1) (x2, ymid)) (jv1) (jv2),
                sz = mergeNodes' (Rect (x1, ymid) (xmid, y2)) (sz1) (sz2),
                sv = mergeNodes' (Rect (xmid, ymid) (x2, y2)) (sv1) (sv2)
            }
            where xmid = div2 (x1 + x2)
                  ymid = div2 (y1 + y2)
     
-- Přijme cílový čtverec, pak nód, který MUSÍ být bodem a vrátí Node, kde je vod v jednom ze čtverců.
-- Na výsledek se nesmí volat packNode, protože by vše vrátil zpět.
-- Funkce se používá uvnitř mergování.

pointToNode :: Numd i => Rect i -> Node i a -> Node i a
pointToNode _ EmpNd = EmpNd
pointToNode rct@(Rect (x1,y1) (x2,y2)) point@(Point (x,y) _) = 
     let vysl =
          if x < x1 || x >= x2 || y < y1 || y >= y2 then error "Mimor rozsah" else
            if (y < ymid) then if (x < xmid)  then Node {jz = point, jv = EmpNd, sz = EmpNd, sv = EmpNd}
                                              else Node {jz = EmpNd, jv = point, sz = EmpNd, sv = EmpNd}
                           else if (x < xmid) then Node {jz = EmpNd, jv = EmpNd, sz = point, sv = EmpNd}
                                              else Node {jz = EmpNd, jv = EmpNd, sz = EmpNd, sv = point}
     --in trace ("poinToNode: " ++ show rct ++ " | " ++ show point ++  " | " ++ show vysl ++ "****" ++ show xmid ++ "*" ++ show ymid) vysl
     in vysl 
    where xmid = div2 (x1 + x2)
          ymid = div2 (y1 + y2)


-- Jeli vstupem čtverec, tak zajistí, že v něm nebude jen jeden bod nebo vše prázdné.
-- Pokud by tomu tak by,lo, tak přímo vytáhne ten nód. Jeden roh obsazený čtvercem však existovat může.
-- Je to vlastně kanonizační funkce, která se vždy aplikue postupně, jak vzniká výsledek.
packNode :: Node i a -> Node i a
packNode (Node EmpNd EmpNd EmpNd EmpNd) = EmpNd
packNode (Node point@(Point _ _) EmpNd EmpNd EmpNd) = point   
packNode (Node EmpNd point@(Point _ _) EmpNd EmpNd) = point   
packNode (Node EmpNd EmpNd point@(Point _ _) EmpNd) = point   
packNode (Node EmpNd EmpNd EmpNd point@(Point _ _)) = point   
packNode node = node

-----------------------------------------------------------------------------------------------------
-- Vnitřní filtrovací funkce pro veškeré typy filtrování dle souřadnic (ne dle dat). Má tyto parametry
-- Pro implementaci funkcí jako ořezávání bodů padnoucích do určitého obrazce.
--   1. Funkce, která rozhodne, zda v z daného obdélníku daném souřadnicemi bereme:
--           všecny body
--           žádné body
--           nějaké body
--  2. Funkce, která rozhodne o konkrétním jednom bodu, zda zahrnout nebo ne
--  3. Obdélník, ze kterého začínáme filtrovat
--  4. Filtrovaný nód.
--  Vrací nód, v němž jsou jen chtěné body.
--

data FilterResult = ALL | SOME | NONE

filterNode :: (Numd i) => (Rect i -> FilterResult) -> ((i,i) -> Bool) -> Rect i -> Node i a -> Node i a 
filterNode _ _ _ EmpNd = EmpNd
filterNode _ fce _ point@(Point xy _) = if fce xy then point else EmpNd
filterNode fce fcePoint rect@(Rect (x1,y1) (x2,y2)) node@(Node jz jv sz sv) =
      let filterResult = fce rect
          xx = case filterResult of
             ALL -> node
             SOME -> 
                    let 
                       xmid = div2 (x1 + x2)
                       ymid = div2 (y1 + y2)
                    in packNode $ Node { 
                        jz = filterNode' (Rect (x1, y1) (xmid, ymid)) jz,
                        jv = filterNode' (Rect (xmid, y1) (x2, ymid)) jv,
                        sz = filterNode' (Rect (x1, ymid) (xmid, y2)) sz,
                        sv = filterNode' (Rect (xmid, ymid) (x2, y2)) sv
                    }
             NONE -> EmpNd  
      in xx
    where filterNode' = filterNode fce fcePoint

-----------------------------------------------------------------------------------------------------
--  Testovací věci
-- 

type Muj = Tree Int Nic
data Nic = Q
  deriving Show

instance Semigroup Nic where
   x <> y = Q

instance Monoid Nic where
   mempty = Q

q1 = pointToTree (31,67) Q :: Muj
q2 = pointToTree (32,66) Q :: Muj
q2x = pointToTree (7,2) Q :: Muj
q2y = pointToTree (7,3) Q :: Muj
q1b = pointToTree (0,0) Q :: Muj
q2b = pointToTree (1,0) Q :: Muj
q3 = q1 <> q2 <> q2x <> q2y :: Muj 
q4 = sameTreeSize q1 q2 :: (Muj, Muj)
q5 = treeRect $ fst q4

qa = pointToNode (Rect (0, 0) (2, 2)) (Point (0,0) Q) :: Node Int Nic
