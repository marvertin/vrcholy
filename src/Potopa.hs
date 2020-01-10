module Potopa
    ( 
      potopaSveta,
      zarovnej,
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

type Hladka = (Mnm, [Moustrov])

-- Bod behem hledání prominence (Bod OStrovni)
data Bost = 
    Kraj -- bod je okrajovým vnitřím bodem ostrova, jenž byl redukován kvůli výkonnosti
  | Pobrezi -- právě přidaný bod, který právě vylezl na hladinu
  | Bost Hladka-- nejvyšší vrcholy ostrova, kte kterému Bost přísluší
  -- deriving (Read)

jeKraj :: Bost -> Bool
jeKraj Kraj = True
jeKraj _ = False

instance Show Bost where
   show Kraj = "."
   show Pobrezi = "_"
   show _ = "*"


jeBost :: Bost -> Bool
jeBost (Bost _) = True
jeBost _ = False

bost2vrch :: Bost -> Hladka
bost2vrch (Bost vrch) = vrch
bost2vrch x = error $ "nelez ziskat vrchol z " ++ (show x)
   

---------------------------------------------------------------------------------
--  Upouštění vody po potopě světa a postupné zaplavování
--
type Sitbo = Sit0 Bost

potopaSveta :: Mnm -> [Bod] -> [Vrch]
potopaSveta minimalniProminence body = potopaSveta' M.empty (rozhladinuj body)
 where
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





