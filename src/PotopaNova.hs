{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PotopaNova
    ( 
      potopaSveta
    ) where

import Lib
import Uzemi (Mou(..), Sit0, okoli0, okoliMou, okoliMouSeMnou)
import VrchTypy (Mnm, Hladina, Vrch(..), Misto(..))

import Data.List
import Data.Maybe
import Data.Tuple
import Data.Ord

import Debug.Trace

import Data.String.Interpolate ( i )
import Text.Printf (printf)

import qualified Data.Map.Strict as M
import qualified Data.Set as S



-- Stav v průbehu výpočtu včetně cílového stavu
--   1. Síť vrcholů, které již vstoupily do sytému Ukazují na svůj "mateřský vrchol", dáli se tomu tak říct, když to nejsou vrcholy.
--      V průběhu zaplavování ukazují na nejvyšší bod ostrova v okamžiku, kdy se toto místo vynořilo nad hladinu.
--      Při shodnosti bodů je to jeden z nich.
--   2. Mapa vrcvholů na své mateřské vrcholy, průběžně se tak buduje pmocí mapy strom (hodnota je dále klíčem atd.)
--      V mapě jsou v klíčích vrcholy, pro které již byl objeven mateřský vrchol, byly tedy začleněny do stromu.
--      Ukládají se sem vrcholy bez ohledu na prominenci. Někdy dokonce může jít jen o spočinek.
--   3. Mapa vrcholů na jejich klíčová sedla. Opět jsou v mapě jen vrcholy u kterých bylo nalezeno klíčové sedlo,
--      což je vždy, když byl také nalezen mateřský vrchol. Ukládají se sem však jen vrcholy s dostatečnou prominencí.
-- Na začátku jsou všechny mapy prázdné, na konci:
--   * je první mapa k ničemu. Měl by tam být jen obrys zpracovávaného území.
--   * třetí mapa má v klíči seznam prominentních vrcholů kromě Sněžky.
--   * druhá mapa obsahuje v klíči všechny vrcholy, kromě Sněžky. Sněžku je zde nutné
--     vyhledat v hodnotách, tak, že se najde rchol, který je v hodnotách a není v klíči.
--  (To všechno platí, když je zpracovávaná oblast souvislá)
data Stav = Stav Optim (Sit0 Misto) (M.Map Misto Misto) (M.Map Misto Misto)
    deriving (Read)


-- Optimalizační data.
data Optim = Optim (S.Set Mou) Int
    deriving (Show, Read)

-- ----------------------------------------------------

-- Připrdne místo nové místo di stavu a vyrobí příští stav.
--  Musí se přidávat místa od nejvyšší po nejnižší nadmořskou výšku, každé
--   přidáme právě jednou a musí tvořit uzavřenou oblast, třeba obdélník, ale může být i nepravidelná.
priprdni :: Int -> Misto -> Stav -> Stav
priprdni minimalniProminence misto@(Misto mnm mou@(Mou x y)) (Stav optim sit mater klised) =
    let 
        okolky = okoli0 sit mou  -- Vše, co je okolo
    in case (okolky) of
        [] -> makeStav (insertToSit misto) mater klised               -- Kolem nic, noří se vrchol nového ostrova
        [jeden] -> makeStav (insertToSit jeden) mater klised  -- Jeden soused, přidá se tedy k tomuto ostrobu
        vice@(prvni : _) -> 
           let (nejvyssi: zbytek) = reverse (samostatneMaterske mater vice)
               vlozDoMap mistecko ktere mapa = (foldr ((flip M.insert) mistecko) mapa ktere) 
           in makeStav (insertToSit prvni) (vlozDoMap nejvyssi zbytek mater) (vlozDoMap misto (filter jeDostProminentni zbytek) klised)
    where 
       --insertToSit misto = optimPromemOkoliNaOkrajeKDyzJsouUzZvnitr mou $ M.insert mou (Miskraj misto) sit
       insertToSit misto = M.insert mou misto sit
       jeDostProminentni (Misto mnmVrcholu _) = mnmVrcholu >= mnm + minimalniProminence
       makeStav = Stav optim


-- Vyhledá zatím osamocené mateřské vrcholy dosažitelné ze zadaných, možná i duplicitních vrcholů
-- 1. paramter je mapa v níž je uložen vlatně les již nalezených vrcholů. Klíčem je vždy vrchol, a hodnotou jeho nalezený mateřský vrchol.
--    Některý vrchol doposud nateřský nemá, tak pro ten klíč není v mapě nic.
--    Aby se algortimus nezacykly musí to být orpavdu strom = (bez cyklů v grafu
--  2. Parametr je seznam výchozích vrcholů z nichž mateřské hledáme, nemusí a mnohdy ani nebudou zatím ve stromu.
--
-- Na místě "m" bude Misto, obecne je to zde proto ,aby se dal jednoduše udělat test této medoy s čísli.

samostatneMaterske :: (Eq m, Ord m) => M.Map m m -> [m] -> [m]
samostatneMaterske mater mistList =  samostatneMaterske' (S.fromList mistList) -- Převodem na množinu zmizela velmi pravděpodobně duplicitní místa, nejčastěji zůstne dokonc jen jedno, to  bude ve stavu, když se nespojují ostrovy
  where
    samostatneMaterske' mista = -- S.Set m -> [m]
         case S.lookupMin mista of
            Nothing -> []
            (Just nejmensi) ->
              let zmensenaMista = S.delete nejmensi mista
              in case M.lookup nejmensi mater of
                  Nothing -> (nejmensi : samostatneMaterske' zmensenaMista) -- Našel jsem zaím nejvyšší maeřský, tak bude ve výsledku a na zbytek použijeme tutéž funkci
                  Just vyssi ->  samostatneMaterske' (S.insert vyssi zmensenaMista)


----------------------------------------------------------------------------------------

inicialniStav = Stav (Optim S.empty 0) M.empty M.empty M.empty

-- 
-- 
potopaSveta :: Mnm -> [Hladina] -> [ Vrch ]
potopaSveta minimalniProminence hladiny =
      let konecnyStav = optimalizujStav $ foldl (flip $ priprdniHladinu minimalniProminence) inicialniStav hladiny
          (Stav _ _ mater klised) = trace [i|Konecny optimalizovany stav: #{konecnyStav} |] konecnyStav
      in  sortOn Down $ map  (\ (vrchol, sedlo) -> Vrch { vrVrchol = vrchol, vrKlicoveSedlo = sedlo, vrMaterskyVrchol = (fromJust $ M.lookup vrchol mater) })  (M.toList klised) 
   where 
     prevedMisto :: Misto -> (Int, (Int, Int))
     prevedMisto (Misto mnm (Mou x y )) = (mnm, (x, y))



priprdniHladinu :: Int -> Hladina -> Stav -> Stav
priprdniHladinu minimalniProminence (mous, mnmVody) stav@(Stav optim sit mater klised) =
  let 
      novyStav = foldr (priprdni minimalniProminence) stav (map (Misto mnmVody) mous) 
      nekdyOptimalizovanyStav = 
          if (vyzadujeOptimalizaci novyStav) 
             then  let optimalizovanyStav = optimalizujStav novyStav
                       kolikratSetresenoSite = fromIntegral(velikostSite novyStav) / fromIntegral(velikostSite optimalizovanyStav) :: Double
                       kolikratSetresenoMater = fromIntegral(velikostMater novyStav) / fromIntegral(velikostMater optimalizovanyStav) :: Double
                       zprava1 = [i|Setreseni site:  #{velikostSite novyStav} => #{velikostSite optimalizovanyStav} to je #{kolikratSetresenoSite} krat |]  :: String
                       zprava2 = [i|Setreseni mater: #{velikostMater novyStav} => #{velikostMater optimalizovanyStav} to je #{kolikratSetresenoMater} krat |]  :: String
                   in trace zprava1 . trace zprava2 $
                      optimalizovanyStav
             else novyStav
      zprava = [i|Hladina: #{mnmVody} mnm #{length mous}  | #{nekdyOptimalizovanyStav} |]
  in trace zprava nekdyOptimalizovanyStav

vyzadujeOptimalizaci :: Stav -> Bool
vyzadujeOptimalizaci (Stav (Optim _ velikostSitePoPosledniOptimalizaci) sit _ _) = M.size sit > 10 * velikostSitePoPosledniOptimalizaci

velikostSite :: Stav -> Int
velikostSite (Stav _ sit _ _) = M.size sit

velikostMater :: Stav -> Int
velikostMater (Stav _ _ mater _) = M.size mater


instance Show Stav where
   show (Stav (Optim okraj lastSitSize) sit mater klised) = [i|#sit= #{M.size sit} #mater=#{M.size mater} #klised=#{M.size klised} #okraj=#{S.size okraj} lastSitSize=#{lastSitSize}|] :: String



-------------------------------------------
--- Globální optimalizace
elemsSet :: Ord a => M.Map k a -> S.Set a
elemsSet = S.fromList . M.elems

zapnoutOptimalizaci = True

optimalizujStav :: Stav -> Stav
optimalizujStav (Stav (Optim okraje lastSitSize) sit mater klised) =
  if zapnoutOptimalizaci then
    let plnky = (M.keysSet  sit) `S.intersection` (majiciPleneObsazeneOkoli okraje sit)  -- To jsou body sítě jejichž okolí je plně obsazené buď bodem nebo okrajem (vnitřním)
        redukovanaSit = sit `M.withoutKeys` plnky               -- Když už má bod obsazeno okolí, není nutné ho uchovávat
        sirsiOkraje = plnky `S.union` okraje                    -- A to co jsme odstranili ze sítě, musíme přidat k vnitřním okrajům a tak je rozšířit
        redukovanaSitSOkolim = S.fromList (M.keys redukovanaSit >>= okoliMouSeMnou) -- JSou to body nové již redukvoané sítě i s okolím. To znamená, že žádný bod, který zde není  se nedostane do blízkosti nové sítě
        posunuteOkraje = sirsiOkraje  `S.intersection` redukovanaSitSOkolim  -- a Stačí zachovat okraje jen ty, které jsou v bezprostřední blískosti sítě pobřeží
        -- Optimalizace stromu mateřských vrcholů, nepotřebujeme ty, které nejsou referenvoány
        -- redukovaneMater = mater `M.restrictKeys` (elemsSet redukovanaSit `S.union` M.keysSet klised `S.union` elemsSet mater)
        redukovaneMater = setresCoToDa (elemsSet redukovanaSit `S.union` M.keysSet klised) mater

    in Stav (Optim posunuteOkraje (M.size redukovanaSit)) redukovanaSit redukovaneMater klised -- vracíme změnšené věci a teké velikost sítě, kterou jsme takto vytvořili
  else Stav (Optim okraje (M.size sit)) sit mater klised

setresCoToDa :: Ord a => S.Set a -> M.Map a a -> M.Map a a
setresCoToDa musiByt mater1 =
     let mater2 =  mater1 `M.restrictKeys` (musiByt `S.union` elemsSet mater1)
     in if M.size mater2 < M.size mater1 then setresCoToDa musiByt mater2
                                         else mater2  


-- Jsou to místa, nemusí nutně tam něco být, pro případ díry by tam nebylo nic
majiciPleneObsazeneOkoli :: (S.Set Mou) -> (Sit0 Misto) -> (S.Set Mou)
majiciPleneObsazeneOkoli okraje sit =  M.keysSet . M.filter (==8) . M.fromListWith (+) . map (\k -> (k, 1)) $ (S.toList okraje ++ M.keys sit) >>= okoliMou
