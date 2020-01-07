module Hledac
    ( 
    zamapuj,
    jeKandidat,
    jeProminentni,
    rozdelNaOstrovy,
    odstranDuplicity,
    potopaSveta
    ) where

import Lib
import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

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
okoliMou mou = mou -:++ []

(-:++) :: Mou -> [Mou] -> [Mou]
(x, y) -:++ moul = (
   (x-1, y-1) :
   (x-1, y) :
   (x-1, y+1) :
   (x, y-1) :
   (x, y+1) :
   (x+1, y-1) :
   (x+1, y) :
   (x+1, y+1) :
   moul
 )
    




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


odstranDuplicity :: [Bod] -> [Bod]           
odstranDuplicity body = map (vystred . M.assocs) ((rozdelNaOstrovy . zamapuj) body)


-- vyrobí bod z prázdného seznamu, který je nějako uprostřed        
vystred :: [Bod] -> Bod
vystred body = (vystredMou (map fst body), (snd . head) body)


       -- vyrobí bod z prázdného seznamu, který je nějako uprostřed        
vystredMou :: [Mou] -> Mou
vystredMou mous =
    let n = length mous
    in (
         (sum $ map fst mous) `div` n,
         (sum $ map snd mous) `div` n
       )




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
      
---------------------------------------------------------------------------------
--  Upouštění vody po potopě světa a postupné zaplavování
--
type Sitbo = Sit0 Bost

potopaSveta :: [Bod] -> [Vrch]
potopaSveta body = potopaSveta' (M.singleton (0,0) (Bost ([(0,0)], 40000 ))) (rozhladinuj body)

potopaSveta' :: Sitbo -> [Hladina] -> [Vrch]
potopaSveta' _ [] = []
potopaSveta' sit (hla : hlaRest) = 
    let
      
      (sitNova, vrcholyHladiny) = trace ("hla " ++ show hla) $ vyresUroven (sit `M.union` (hladinaToSit hla)) (snd (traceShowId hla))
      vrcholySpodnejsich = potopaSveta' sitNova hlaRest
    in vrcholyHladiny ++ vrcholySpodnejsich
  where
    -- vstupem je 
    --    1. síť ostrovů po ustoupení vody o 1 metr, kdy se právě vynořilo "Pobrezi" ještě bez vrcholů
    --    2. nadmořská výška pobřeží
    -- výstupem je:
    --    1. síť upravená tak, že pobřeží získá svůj vrch na svých ostrovech a ntitřky jsou kvůli optimalizaci vymazané
    --       Tato síť neobdsahuje žádné Pobrezi, bude však obsahovat Kraj
    --    2. seznam vrcholů, které se staly vrcholy ostrovů právě spojených s ostrovem s vyšším vrcholem
    vyresUroven :: Sitbo -> Mnm -> (Sitbo, [Vrch])
    vyresUroven sit mnm = 
      let ostrovy = trace ("sitka je " ++ show sit) ( rozdelNaOstrovy sit)
      --in  (M.empty, [])
      in foldl accumOstrov (M.empty, []) ostrovy
        where
          accumOstrov :: (Sitbo, [Vrch]) -> Sitbo -> (Sitbo, [Vrch])
          accumOstrov (accumSit, accumVrcholy) ostrov =
            let (ostrovSit, ostrovVrcholy) = vyresOstrov ostrov mnm
            in (accumSit `M.union` ostrovSit, ostrovVrcholy ++ accumVrcholy)

    -- Totéž co vyresUroven, ale resi pro jeden ostrov
    vyresOstrov :: Sitbo -> Mnm -> (Sitbo, [Vrch])  
    vyresOstrov sit mnm = 
      -- vrskoMapa má klíče nadmořské výšky všech vrcholů již dříve nalezených na ostrovech
      -- hodnoty jsou pak seznamy hladin, které byly u každého bodu
      let vrskoMapa = grupuj (snd . bost2vrch) bost2vrch $ filter jeBost (M.elems sit)
          vcholky = najdiVrcholy (M.deleteMax vrskoMapa)
          novaSit = zarovnej ( (head . snd) (M.findMax vrskoMapa)) sit -- to bude nejvyšší bod ostrova

      in (novaSit, vcholky) 
       where
          -- Dostáváme vrchloy právě sloučen=ho ostrova ale s odříznutým
          -- nejvyšším vrcholem. V mapě je tolik hodnot, kolik máme vrchoů nejvyšších bodů
          -- vše co je v seznamu hladin je stejné
          najdiVrcholy :: M.Map Mnm [Hladina] -> [Vrch]
          najdiVrcholy vrsici = 
                  map (\(vyska, ( (mous, _) : _)) ->  ( ( vystredMou mous, vyska ), ([], mnm), [] )) (M.toList vrsici) --  ( 1602, [([(1,3),(4,8) ...], 1602)...])

    zarovnej :: Hladina -> Sitbo ->  Sitbo
    zarovnej vrchol sit =  kolona sit
       where
         kolona = M.fromList . map (doplnVrchol vrchol) . filter filtrujKraje . map nahradVnitrni . M.toList

         nahradVnitrni :: (Mou, Bost) -> (Mou, Bost)
         nahradVnitrni bod@(_, Kraj) = bod  -- kraje necháváme, je to optimalizace at nehledáme zbytečně
         nahradVnitrni bod@(mou, bost)
           | jeVnitrnimBodem mou = (mou, Kraj)
           | otherwise = bod
         
         filtrujKraje :: (Mou, Bost) -> Bool
         filtrujKraje (mou, Kraj) = maJenKrajeKolem mou
         filtrujKraje _ = True

         doplnVrchol :: Hladina -> (Mou, Bost) -> (Mou, Bost)
         doplnVrchol vrchol (mou, _) = (mou, Bost vrchol)

         jeVnitrnimBodem = all (flip M.member sit) . okoliMou
         maJenKrajeKolem = (all jeKraj) . catMaybes . map (flip M.lookup sit) . okoliMou


hladinaToSit :: Hladina -> Sitbo
hladinaToSit hladina = M.fromList (map (\mou -> (mou, Pobrezi)) (fst hladina)  )

-- rozdělí celou síť na hladiny
rozhladinuj :: [Bod] -> [Hladina]
rozhladinuj = reverse . map swap . M.toList . grupuj snd fst 





