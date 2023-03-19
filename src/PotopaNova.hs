module PotopaNova
    ( 
      potopaSveta,
      potopaSvetaZBodu,
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

potopaSvetaZBodu :: Mnm -> [Bod] -> [Vrch]
potopaSvetaZBodu minimalniPromnence  body = potopaSveta minimalniPromnence (rozhladinuj body)


potopaSveta :: Mnm -> [Hladina] -> [Vrch]
potopaSveta minimalniProminence hladiny = potopaSveta' M.empty hladiny
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
      vyresUroven sit mnmVody = 
        let ostrovy = ( rozdelNaOstrovy sit)
            vysl@(sit2, vrycholy) = foldl accumOstrov (M.empty, []) ostrovy
            zprava = "Hladina: " ++ show mnmVody ++ "   ostrovu: " ++  (show.length) ostrovy ++ "  sit: " ++ (show.M.size) sit ++ " ==> " ++ (show.M.size) sit2 ++ "  vrcholy: " ++ (show.length) vrycholy
        in trace zprava vysl
          where
            accumOstrov :: (Sitbo, [Vrch]) -> Sitbo -> (Sitbo, [Vrch])
            accumOstrov (accumSit, accumVrcholy) ostrov =
              let (ostrovSit, ostrovVrcholy) = vyresOstrov ostrov mnmVody
              in (accumSit `M.union` ostrovSit, ostrovVrcholy ++ accumVrcholy)

      -- Totéž co vyresUroven, ale resi pro jeden ostrov
      vyresOstrov :: Sitbo -> Mnm -> (Sitbo, [Vrch])  
      vyresOstrov sit mnmVody = 
        let
            -- Podostrovy rekonstruují původní ostrovy, je jich přesně tolik, kolik ostrovů se právě spojilo. 
            --   Klíčem je hladina nejvyšších vrcholů původních ostrovů. Pokud se právě spojilo
            --      více stejně vysokých ostrovů, je v klíči více moustrovů se stejnou výškou
            --   Hodnotou je množina souřadnic ostrova (vlastně jsout to jen jeho okraje)
            podostrovyMapa :: M.Map Hladka [Mou]
            podostrovyMapa = grupuj (bost2vrch . snd) fst $ filter (jeBost . snd) (M.toList sit)

            -- Tdy jsou podostrovy jako seznam
            podostrovy :: [(Mnm, [Moustrov], [Mou])] -- nadmořská výška vrcholů, seznam vrcholů o této výšce, okrajové body ostrova
            podostrovy = map (\ ((mnm, vrchy), okraje) -> (mnm, vrchy, okraje) ) . M.toList $ podostrovyMapa
            
            pobrezi = M.keys $ M.filter (not . jeBost) sit

            in if null podostrovy then
                    let 
                        -- vynoření špiček ostrova, stávají se základem ostrova a později nejvyšším vrcholem
                        vrnci = Moustrov $ M.keys sit
                        novaSit = M.map (const $ Bost (mnmVody, [vrnci]))  sit
                    in (novaSit, [])
                else    
                    
                    let 
                        maximMnm = fst3 (maximum podostrovy) -- spoléháme na to, že výška je na prvním místě
                        (nejvyssi, nizsi) = partition (\q -> fst3 q == maximMnm) podostrovy
                        sloucenyNejvyssi = (maximMnm, nejvyssi >>= snd3)

                        materskyVrchol = Kopec maximMnm ((head.snd3.head) nejvyssi) -- je to libovolny z tech vyččích vrcholů

                        ostruvky = map (\ ( (mnm, vrcholy, okraje)  : zbytekOstrovu)  ->
                                (mnm, vrcholy, nejkratsiSpoj okraje pobrezi (zbytekOstrovu ++ nejvyssi >>= thr3) )
                            ) . filter (\ ((vyska, _, _) : _) -> vyska - mnmVody > minimalniProminence) $ -- filtrujeme aspon trochu prominentni
                              (kazdyPrvekAZbytky nizsi)
                        
                        novaSit = zarovnej sloucenyNejvyssi sit -- to bude nejvyšší bod ostrova
                        vrcholx = najdiVrcholx ostruvky materskyVrchol
                    in (novaSit, vrcholx) 

        where
            -- dostavame cely ostruvek s výškoku, vrcholkama a nejvyšším sedlem
            najdiVrcholx :: [(Mnm, [Moustrov], Moustrov)] -> Kopec -> [Vrch]
            najdiVrcholx ostruvky materskyVrchol = 
                let 
                  kopecky = zOstruvkuKopecky ostruvky
                in  map (\(kopecek, klicoveSedlo) ->  Vrch {vrVrchol = kopecek,
                                                            vrKlicoveSedlo = Kopec mnmVody klicoveSedlo, 
                                                            vrMaterskeVrcholy = materskyVrchol }) kopecky

zOstruvkuKopecky :: [(Mnm, [Moustrov], Moustrov)] -> [(Kopec, Moustrov)]
zOstruvkuKopecky ostruvky = ostruvky >>= (\ (mnm, vrcholky, klicoveSedlo)  ->  map  (\kopec ->  (Kopec mnm kopec, klicoveSedlo) ) vrcholky) 


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





