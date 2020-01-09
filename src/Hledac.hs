module Hledac
    ( 
    zamapuj,
    jeKandidat,
    jeProminentni,
    rozdelNaOstrovy,
    odstranDuplicity,
    potopaSveta,
    zarovnej,
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


-- vyrobí bod z neprázdného seznamu, který je nějako uprostřed        
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
potopaSveta body = potopaSveta' M.empty (rozhladinuj body)

potopaSveta' :: Sitbo -> [Hladina] -> [Vrch]
potopaSveta' _ [] = []
potopaSveta' sit (hla : hlaRest) = 
    let
      
      (sitNova, vrcholyHladiny) = vyresUroven (sit `M.union` (hladinaToSit hla)) (snd hla)
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
      let ostrovy = ( rozdelNaOstrovy sit)
      --in  (M.empty, [])
          vysl@(sit2, vrycholy) = foldl accumOstrov (M.empty, []) ostrovy
          zprava = "Hladina: " ++ show mnm ++ "   ostrovu: " ++  (show.length) ostrovy ++ "  sit: " ++ (show.M.size) sit ++ " ==> " ++ (show.M.size) sit2 ++ "  vrcholy: " ++ (show.length) vrycholy
      in trace zprava vysl
        where
          accumOstrov :: (Sitbo, [Vrch]) -> Sitbo -> (Sitbo, [Vrch])
          accumOstrov (accumSit, accumVrcholy) ostrov =
            let (ostrovSit, ostrovVrcholy) = vyresOstrov ostrov mnm
            in (accumSit `M.union` ostrovSit, ostrovVrcholy ++ accumVrcholy)

    -- Totéž co vyresUroven, ale resi pro jeden ostrov
    vyresOstrov :: Sitbo -> Mnm -> (Sitbo, [Vrch])  
    vyresOstrov sit mnm = 
      let
          -- Podostrovy rekosnstruují původní ostrovy, je jich přesně tolik, kolik ostrovů se právě spojilo. 
          --   Klíčem je hladina nejvyšších vrcholů původních ostrovů. Pokud se právě spojilo
          --      více stejně vysokých ostrovů, je v klíči více hladin se stejnou výškou
          --   Hodnotou je množina souřadnic ostrova (vlastně jen jeho okraje)
          podostrovy :: M.Map Hladka [Mou]
          podostrovy = grupuj (bost2vrch . snd) fst $ filter (jeBost . snd) (M.toList sit)
          -- vrskoMapa má klíče nadmořské výšky nejvyšších vrcholů všech spojovasných ostrovů
          -- hodnoty jsou pak seznamy hladin vrcholů původních ostrovů. Typicky je zde jedna hodnota.
          --  Pokud se spojovalo více ostrovů o stejné výšce, tak je jich více.
          -- počet prvků mapy odpovídá počtu právě spojených ostrovů (nebo méně, pokud byly některé spojované ostrovy stejně vysoké),
          --    ale po rozvinutí seznamů by počet prvků odpovídal přesně počtu ostrovů
          vrskoMapa :: M.Map Mnm [Moustrov]
          vrskoMapa = M.map concat $ grupuj fst snd (M.keys podostrovy)

      in if (M.null vrskoMapa) then
                  let 
                      -- vynoření špiček ostrova, stávají se základem ostrova a později nejvyšším vrcholem
                      vrnci = Moustrov $ M.keys sit
                      novaSit = M.map (const $ Bost (mnm, [vrnci]))  sit
                  in (novaSit, [])
              else    
                  let (nejvyssiMnm, moumous) = M.findMax vrskoMapa
                      novaSit = zarovnej (nejvyssiMnm, moumous) sit -- to bude nejvyšší bod ostrova
                      vcholky = najdiVrcholy (M.deleteMax vrskoMapa)
                      in (novaSit, vcholky) 

       where
          -- Dostáváme to co je ve vrskoMapa, ale s odříznutým nejvyšším vrcholem, neboť ten teď
          -- neřešíme nebo nemůžeme určit jeho klíčové sedlo v tomto okamžiku
          najdiVrcholy :: M.Map Mnm [Moustrov] -> [Vrch]
          najdiVrcholy vrsici = 
              let 
                asponTrochuProminentniVrsici = filter (\ (vyska, _) -> vyska - mnm > minimalniProminence)  (M.toList  vrsici)
                vrchyJakoBody = map (\(mnm, moustrov) -> Kopec mnm moustrov) $ asponTrochuProminentniVrsici >>= rozbal2;
              in  map (\bod ->  Vrch {vrVrchol = bod,
                                     vrKlicoveSedlo = Kopec mnm klicoveSedlo, 
                                     vrMaterskeVrcholy = materskeVrcholy }) vrchyJakoBody

-- TODO mateřský island vrchol spočítat                  
materskeVrcholy :: Kopec
materskeVrcholy = Kopec 0 (Moustrov [])

-- TODO klíčové sedlo spočítat
klicoveSedlo :: Moustrov
klicoveSedlo = Moustrov []



zarovnej :: Hladka -> Sitbo ->  Sitbo
zarovnej vrchol sit = 
      let sit2 = M.mapWithKey (nahradVnitrni sit) sit  
      in M.map nahradVrchol . M.filterWithKey (filtrujKraje sit2) $ sit2
    where

      nahradVnitrni :: Sitbo -> Mou -> Bost -> Bost
      nahradVnitrni _ _ Kraj = Kraj  -- kraje necháváme, je to optimalizace at nehledáme zbytečně
      nahradVnitrni sit1 mou bost
        | all (flip M.member sit1) . okoliMou $ mou = Kraj
        | otherwise = bost
      
      filtrujKraje :: Sitbo -> Mou -> Bost -> Bool
      filtrujKraje sit2 mou Kraj = not . (all jeKraj) . catMaybes . map (flip M.lookup sit2) . okoliMou $ mou
      filtrujKraje _ _ _ = True

      nahradVrchol :: Bost -> Bost
      nahradVrchol Kraj = Kraj
      nahradVrchol _  = Bost vrchol



hladinaToSit :: Hladina -> Sitbo
hladinaToSit hladina = M.fromList (map (\mou -> (mou, Pobrezi)) (fst hladina)  )

-- rozdělí celou síť na hladiny
rozhladinuj :: [Bod] -> [Hladina]
rozhladinuj = reverse . map swap . M.toList . grupuj snd fst 





