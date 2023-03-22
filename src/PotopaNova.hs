{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PotopaNova
    ( 
      potopaSveta,
      potopaSvetaZBodu,
      xxx, kus, unique
    ) where

import Lib
import Uzemi
import VrchTypy

import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

import Data.String.Interpolate ( i )
import Text.Printf (printf)

import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- Místo je pozoce na mapě spolu s nadmořskou výškou
-- Řadíme nejprve dle nadmořské výšky, pak už je to jedno, podle které souřadnice
data Misto = Misto Mnm Mou 
  deriving (Read, Show, Eq, Ord)

-- Místo nebo okraj. Myšleno vnitřní okraj. Tato možnsot je zde kvůli optimalizaci, aby
--    se tak rychle nespotřebobábala paměť. Jak se ostrovy vynořují, tak vnitřní nebudou potřeba,
--    proto si držímě vnitřní kraj a jak se ostro vynořuje, kraj je stlá více uvnitř ostrova a zaniká.
--    Víme totiž, že každý bod přijde práv jendnou, tskže nic sousedního nebude.
data Miskraj = Miskraj Misto | Okraj
   deriving (Read, Show)

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
data Stav = Stav (Sit0 Miskraj) (M.Map Misto Misto) (M.Map Misto Misto)
    deriving (Read)


data DebugObsah = NIC_ | KRAJ | VRCH
    deriving (Read, Show)

debugMiskraj :: Maybe Miskraj -> DebugObsah
debugMiskraj Nothing = NIC_
debugMiskraj (Just Okraj) = KRAJ
debugMiskraj (Just (Miskraj _)) = VRCH

debugOkoli :: (Sit0 Miskraj) -> Mou -> [(Mou, DebugObsah)]
debugOkoli sit mou =
   let okoli = okoliMou mou
   in  map (\m -> (m, debugMiskraj (M.lookup m sit))  ) okoli


-- ----------------------------------------------------

-- Připrdne místo nové místo di stavu a vyrobí příští stav.
--  Musí se přidávat místa od nejvyšší po nejnižší nadmořskou výšku, každé
--   přidáme právě jednou a musí tvořit uzavřenou oblast, třeba obdélník, ale může být i nepravidelná.
priprdni :: Int -> Misto -> Stav -> Stav
priprdni minimalniProminence misto@(Misto mnm mou@(Mou x y)) (Stav sit mater klised) =
    let 
        okolky = nekrajf $ okoli0 sit mou  -- Vše, co je okolo
    in case (okolky) of
        [] -> Stav (insertToSit misto) mater klised               -- Kolem nic, noří se vrchol nového ostrova
        [jeden] -> Stav (insertToSit jeden) mater klised  -- Jeden soused, přidá se tedy k tomuto ostrobu
        vice@(prvni : _) -> 
           let (nejvyssi: zbytek) = reverse (samostatneMaterske mater vice)
               vlozDoMap mistecko ktere mapa = (foldr ((flip M.insert) mistecko) mapa ktere) 
           in Stav (insertToSit prvni) (vlozDoMap nejvyssi zbytek mater) (vlozDoMap misto (filter jeDostProminentni zbytek) klised)
    where 
       --insertToSit misto = optimPromemOkoliNaOkrajeKDyzJsouUzZvnitr mou $ M.insert mou (Miskraj misto) sit
       insertToSit misto = M.insert mou (Miskraj misto) sit
       jeDostProminentni (Misto mnmVrcholu _) = mnmVrcholu >= mnm + minimalniProminence


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



nekraj :: Miskraj -> Misto
nekraj (Miskraj misto) = misto
nekraj Okraj = error "Je tam OKRAJ!!!"

nekrajf :: [Miskraj] -> [Misto]
nekrajf mista =  if any jeKraj mista then error ("Je tam Okraj: " ++ show mista)
                                     else map nekraj mista

----------------------------------------------------------------------------------------


optimPromemOkoliNaOkrajeKDyzJsouUzZvnitr :: Mou -> (Sit0 Miskraj) -> (Sit0 Miskraj) 
optimPromemOkoliNaOkrajeKDyzJsouUzZvnitr mou sit =
     let okolicko = okoliMou mou :: [Mou]
         okolickoRozsirene = if length okolicko == 8 then (mou : okolicko) else okolicko -- Případ, kdy je obklopeno vše u vkládaného je vzácný, proto speciálně ošetřen
     in foldr optimPromemNaOkrajKdyzCelaObklopena sit okolickoRozsirene 



-- Pokud je daná buňka zcela obklopena ímkoli (ale neprázdné), promění ji na okraj
-- a pak smaže všechny okraje v okolí obklopené jen okraji
optimPromemNaOkrajKdyzCelaObklopena :: Mou -> (Sit0 Miskraj) -> (Sit0 Miskraj)
optimPromemNaOkrajKdyzCelaObklopena mou sit =
     let okolicko = okoliMou mou :: [Mou]
     in
        if not (any isNothing $ map (flip M.lookup sit) okolicko)  then  -- všechno v okolí je zabráno
            let jenOkrajeVOkoli = filter (jeKrajXY sit) okolicko
            in foldr optimSmazKdyzJenMeziOkraji (M.adjust (const Okraj) mou sit) (mou : jenOkrajeVOkoli) -- také centrální právě předělána se může vyskytnout mezi okraji
        else sit
    

-- Pokud je buňka mezi samými okraji nebo ničím, bude smazána. A bez ohledu na její obsah či existenci.
optimSmazKdyzJenMeziOkraji :: Mou -> (Sit0 Miskraj) -> (Sit0 Miskraj)
optimSmazKdyzJenMeziOkraji mou sit = 
    if all jeKraj (okoli0 sit mou) then M.delete mou sit
                                   else sit 

dejMou :: Miskraj -> Mou
dejMou (Miskraj (Misto _ mou) ) = mou

jeKrajXY ::  (Sit0 Miskraj) -> Mou -> Bool
jeKrajXY sit mou =  case M.lookup mou sit of 
                        Nothing -> False
                        Just Okraj -> False
                        _ -> True

jeKraj :: Miskraj -> Bool
jeKraj Okraj = True
jeKraj _ = False

jeMisto :: Miskraj -> Bool
jeMisto (Miskraj _)  = True
jeMisto _ = False

---------------------------------------------------

mapolist = [(10,20), (11,20), (20,30), (12,22), (13,22), (14,22), (22,30), (15,25), (16,26), (26,36), (36,46), (17,27), (18,27)]
mt1 = M.fromList mapolist

---------------------------------------------------

potopaSvetaZBodu :: Mnm -> [Bod] -> [Vrch]
potopaSvetaZBodu minimalniPromnence  body = []

inicialniStav = Stav M.empty M.empty M.empty

potopaSveta :: Mnm -> [Hladina] -> [Vrch]
potopaSveta minimalniProminence hladiny =
       sort . convertNaStare . fst $ foldr (priprdniHladinu minimalniProminence) (inicialniStav, 0) (reverse hladiny)

priprdniHladinu :: Int -> Hladina -> (Stav, Int) -> (Stav, Int)
priprdniHladinu minimalniProminence (mous, mnmVody) (stav@(Stav sit mater klised), velikostSitePoPosledniOptimalizaci) =
  let 
      novyStav@(Stav sit _ _) = foldr (priprdni minimalniProminence) stav (map (Misto mnmVody) mous) 
      (nekdyOptimalizovanyStav, novaVelikost) = 
          if (M.size sit > 5 * velikostSitePoPosledniOptimalizaci) 
             then  let optimalizovanyStav@(Stav sit2 _ _) = optimalizujStav novyStav
                       kolikratSetreseno = fromIntegral(M.size sit) / fromIntegral(M.size sit2) :: Double
                   in trace [i|Setreseni site: #{M.size sit} => #{M.size sit2} to je #{kolikratSetreseno} krat|] (optimalizovanyStav, M.size sit2)
             else (novyStav, velikostSitePoPosledniOptimalizaci)
      zprava = [i|Hladina: #{mnmVody} mnm #{length mous}  | #{nekdyOptimalizovanyStav} |]
  in trace zprava (nekdyOptimalizovanyStav, novaVelikost)

instance Show Stav where
   show (Stav sit mater klised) = [i|#sit= #{M.size sit} #mater=#{M.size mater} #klised=#{M.size klised}|] :: String


convertNaStare :: Stav -> [Vrch]
convertNaStare (Stav _ mater klised) =
     
     map prevedNaVrch (M.toList klised)
   where 
      prevedNaVrch (vrchol, sedlo) = Vrch {
            vrVrchol = misto2kopec vrchol,
            vrKlicoveSedlo = misto2kopec sedlo,
            vrMaterskeVrcholy = misto2kopec $ fromJust $ M.lookup vrchol mater
          }

      


misto2kopec :: Misto -> Kopec
misto2kopec (Misto mnm mou) = Kopec mnm (Moustrov [mou])



-------------------------------------------
--- Pokus o globálí optimalizaci

--
-- Vybere ze sezanm,u prvky, které jsou tam v minimálním počtu za sebou.
-- Pro každou skupin větší nebo rovnou zadanému číslu vybere prvek tvořící skupinu.
-- Řdí zezadu, nefunguje pro menší nebo rovno 2.
--   vyberSMinimalneVyskyty 3 "111aabb2222cc333333333d111xyzz444j" = "41321"
--  
vyberSMinimalneVyskyty :: (Show a, Eq a) => Int -> [a] -> [a]
vyberSMinimalneVyskyty _ [] = []
vyberSMinimalneVyskyty n (prvni: zbytek) = 
     let (_, _, vysl) = foldl' akum (prvni, 1, []) zbytek
     in vysl
  where
      akum (last, minulyPocet, vysl) x = 
         if x == last then 
                           let pocet = minulyPocet + 1
                           in (last, pocet, if pocet == n then  (last: vysl) else  vysl)
                      else     
                            (x, 1, vysl)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : lx) = (x : unique (dropWhile (==x) lx))


majiciPleneObsazeneOkoli :: (Sit0 a) -> [Mou]
majiciPleneObsazeneOkoli sit = vyberSMinimalneVyskyty 8 $ sort $ M.keys sit >>= okoliMou

nahradObsazeneKrajem :: (Sit0 Miskraj) -> (Sit0 Miskraj) 
nahradObsazeneKrajem sit = foldr (M.adjust (const Okraj)) sit (majiciPleneObsazeneOkoli sit)

-- Jen tam kde už jsou místa, ale ne kraje
-- partionMistoOkraj :: (Sit0 Miskraj) -> ([Mou], [Mou])
-- partionMistoOkraj sit = partition (jeMisto . snd) (M.toList sit)

vymazNesousedniKraje :: (Sit0 Miskraj) -> (Sit0 Miskraj) 
vymazNesousedniKraje sit = 
  let 
      mista = map fst $ filter (jeMisto . snd) (M.toList sit)
      okoliMist = unique $ sort $ mista >>= okoliMouSeMnou -- okolí míst, ty musí zůstat, pokud tam jsou kraje
  in M.restrictKeys sit (S.fromList okoliMist)

optimalizuj :: (Sit0 Miskraj) -> (Sit0 Miskraj) 
optimalizuj = vymazNesousedniKraje . nahradObsazeneKrajem

optimalizujStav :: Stav -> Stav
optimalizujStav (Stav sit mater klised) = Stav (optimalizuj sit) mater klised


mik = Miskraj (Misto 0 (Mou 0 0))

-- kus = M.fromList [(Mou 5 12, mik), (Mou 5 14, mik), (Mou 5 13, mik), (Mou 3 13, mik), (Mou 4 13, mik), (Mou 6 13, mik)]
kus = M.fromList [(Mou 5 12, mik), (Mou 5 14, mik), (Mou 5 13, mik), (Mou 4 13, mik)]
xxx =  nahradObsazeneKrajem $ kus
