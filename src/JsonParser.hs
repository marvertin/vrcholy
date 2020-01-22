{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module JsonParser
    (   
      Json(..),
      parse,
      jsonToString
    ) where
 
import Control.Arrow
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map.Lazy as M


data Json = Obj (M.Map String Json) | Arr [Json] |
              Str String | Num Integer | Boo Bool | Nul 
 deriving (Show, Read, Eq)


parse :: String -> Json
parse = snd . pars


pars :: String -> (String, Json)
pars (x: r) | isSpace x = pars r
pars ('[': r) = fmap Arr $ parsArr r
pars ('{': r) = fmap Obj $ parsObj r
pars ('\'': r) = fmap Str $ parsStr r
pars ('t': 'r' : 'u' : 'e': r) = (r, Boo True)
pars ('f': 'a' : 'l' : 's': 'e': r) = (r, Boo False)
pars ('n': 'u' : 'l': 'l': r) = (r, Nul)
pars r@('0': _) = fmap Num $ parsNumber r
pars r@('1': _) = fmap Num $ parsNumber r
pars r@('2': _) = fmap Num $ parsNumber r
pars r@('3': _) = fmap Num $ parsNumber r
pars r@('4': _) = fmap Num $ parsNumber r
pars r@('5': _) = fmap Num $ parsNumber r
pars r@('6': _) = fmap Num $ parsNumber r
pars r@('7': _) = fmap Num $ parsNumber r
pars r@('8': _) = fmap Num $ parsNumber r
pars r@('9': _) = fmap Num $ parsNumber r
pars ('-': r) = fmap Num $ let (rx, num) = parsNumber r in (rx, -num)
pars txt = error $ "syntax error on \"" ++ take 50 txt ++ "...\""

parsArr :: String -> (String, [Json])
parsArr (x: r) | isSpace x = parsArr r
parsArr (']': r) = (r, [])
parsArr (',': r) = parsArr r
parsArr r = let (rx, jj) = pars r
                (ry, jjs)  = parsArr rx
            in (ry, (jj : jjs))

parsNumber :: String -> (String, Integer)
parsNumber r = parsNumber' 0 r

parsNumber' :: Integer -> String -> (String, Integer)
parsNumber' nn [] = ([], nn)
parsNumber' nn rr@(z: r) = let num = ord z - ord '0'
                           in if num < 0 || num > 9 then (rr, nn)
                                                    else  parsNumber' (nn * 10 + fromIntegral num) r
                                                        

--(tail x, Num $ read [head x])

parsObj :: String -> (String, M.Map String Json)
parsObj (x: r) | isSpace x = parsObj r
parsObj ('}' : r) = (r, M.empty)
parsObj (',': r) = parsObj r
parsObj ('\'' : r) = let
    (rx, klic) = parsStr r
    (ry, jj) = pars (expectColon rx)
    (rz, jjm) = parsObj ry
    in (rz, M.insert klic jj jjm)

parsStr :: String -> (String, String)
parsStr ('\'': r) = (r, "")
parsStr ('\\': r) = parsStr' r
parsStr r = parsStr' r

parsStr' [] = error "necekany konec vstupnicho textu"
parsStr' (z: r) = let (rx, zs) = parsStr r
                  in (rx, z : zs) 

expectColon :: String -> String
expectColon (x: r) | isSpace x = expectColon r
expectColon [] = error $  "ocekavana dvojtecka, ale je konec vstupu"
expectColon (':': r) = r
expectColon r = error $  "ocekavana dvojtecka: " ++ r


dropWhites :: String -> String
dropWhites [] = []
dropWhites (' ' : rest) = dropWhites rest
dropWhites x = x


jsonToString :: Json -> String
jsonToString = unlines . jsonToString'

jsonToString' :: Json -> [String]
jsonToString' = j2s 
  where
    j2s Nul =  ["null"]
    j2s (Boo True) = ["true"]
    j2s (Boo False) = ["false"]
    j2s (Num n) = [show n]
    j2s (Str s) = [show s]
    j2s (Arr a) = ["["] ++ odsad (a >>= jsonToString')  ++ ["]"]
    j2s (Obj a) = ["{"] ++ odsad (M.toList a >>= (\(klic, jj) -> ( (klic ++ ":" ) : (odsad . jsonToString' ) jj)))  ++ ["}"]

    odsad :: [String] -> [String]
    odsad s = map ("   " ++) s
    