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
    deriving (Read, Show)



-- ----------------------------------------------------

-- Připrdne místo nové místo di stavu a vyrobí příští stav.
--  Musí se přidávat místa od nejvyšší po nejnižší nadmořskou výšku, každé
--   přidáme právě jednou a musí tvořit uzavřenou oblast, třeba obdélník, ale může být i nepravidelná.
priprdni :: Misto -> Stav -> Stav
priprdni misto@(Misto mnm mou@(Mou x y)) (Stav sit mater klised) =
   let  okolky = nekrajf (okoli0 sit mou)  -- Vše, co je okolo
   in case (okolky) of
        [] -> Stav (insertToSit misto) mater klised               -- Kolem nic, noří se vrchol nového ostrova
        [jeden] -> optim mou $ Stav (insertToSit jeden) mater klised  -- Jeden soused, přidá se tedy k tomuto ostrobu
        vice@(prvni : _) -> 
           let (nejvyssi: zbytek) = reverse (samostatneMaterske mater vice)
               vlozDoMap mistecko mapa = (foldr ((flip M.insert) mistecko) mapa zbytek) 
           in optim mou $ Stav (insertToSit prvni) (vlozDoMap nejvyssi mater) (vlozDoMap misto klised)
    where 
       insertToSit misto = M.insert mou (Miskraj misto) sit

-- Seznam je zaručeně neprázdný
materuj :: [Misto] -> M.Map Misto Misto -> M.Map Misto Misto
materuj mista mater = 
  case  reverse (samostatneMaterske mater mista) of -- Samostatné kopce od nejvyššího po nejnižší
    [] -> mater -- pro úplnost, to by nemělo ale nastat
    [_] -> mater -- jeden prvek bude velmi častý, což znamená, že jen přidáváme ke stávajícímu ostrovu a nespojujeme (nemuselo by být, zpracoval by další řádek, ale je to názornější)
    (nejvyssi: zbytek) -> foldr ((flip M.insert) nejvyssi) mater zbytek -- tady dochází ke spojení ostrovů, čímž vlastně vnikají vrcholy
      
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


optim :: Mou -> Stav -> Stav   
optim _ = id


nekraj :: Miskraj -> Misto
nekraj (Miskraj misto) = misto

nekrajf :: [Miskraj] -> [Misto]
nekrajf = map nekraj 
---------------------------------------------------

mapolist = [(10,20), (11,20), (20,30), (12,22), (13,22), (14,22), (22,30), (15,25), (16,26), (26,36), (36,46), (17,27), (18,27)]
mt1 = M.fromList mapolist

---------------------------------------------------

potopaSvetaZBodu :: Mnm -> [Bod] -> [Vrch]
potopaSvetaZBodu minimalniPromnence  body = []

inicialniStav = Stav M.empty M.empty M.empty

potopaSveta :: Mnm -> [Hladina] -> [Vrch]
potopaSveta minimalniProminence hladiny =
       sort $ convertNaStare minimalniProminence $ foldr priprdniHladinu inicialniStav (reverse hladiny)

priprdniHladinu :: Hladina -> Stav -> Stav
priprdniHladinu (mous, mnmVody) stav@(Stav sit mater klised) =
  let zprava = "Hladinarza: " ++ show mnmVody ++ " mnm " ++ (show . length) mous ++ " | sit: " ++  (show . M.size) sit ++ " mater: " ++  (show . M.size) mater ++ " klised: " ++  (show . M.size) klised
  in  foldr priprdni (trace zprava stav) (map (Misto mnmVody) mous)

convertNaStare :: Mnm -> Stav -> [Vrch]
convertNaStare minimalniProminence (Stav _ mater klised) =
     
     map prevedNaVrch
     $ filter jeMinimalniProminence -- filtrovat jen vrcholy s minimální prominencí
     (M.toList klised)
   where 
      prevedNaVrch (vrchol, sedlo) = Vrch {
            vrVrchol = misto2kopec vrchol,
            vrKlicoveSedlo = misto2kopec sedlo,
            vrMaterskeVrcholy = misto2kopec $ fromJust $ M.lookup vrchol mater
          }

      jeMinimalniProminence ((Misto vyskaVrcholu _), (Misto vyskaSedla _)) = vyskaVrcholu >= vyskaSedla + minimalniProminence


misto2kopec :: Misto -> Kopec
misto2kopec (Misto mnm mou) = Kopec mnm (Moustrov [mou])


-- zprava = "Hladina: " ++ show mnmVody ++ "   ostrovu: " ++  (show.length) ostrovy ++ "  sit: " ++ (show.M.size) sit ++ " ==> " ++ (show.M.size) sit2 ++ "  vrcholy: " ++ (show.length) vrycholy