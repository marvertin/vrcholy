{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Nacitac
import ZapisovacVrchSvg
import Uzemi
import VrchTypy
import PripravaVstupu
import Konst
import GeonamesParser
import JsonParser
import GeonamesIo

import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import GHC.IO.Encoding
import Network.Wreq
import Control.Lens
import Control.Monad

main :: IO ()
main = join $ liftM2 vyrobSvg ggfile2 ggfile4vrchSvg

prořeď :: Int -> [a] -> [a]
prořeď _ [] = []
prořeď n list
     | n < 0 = list
prořeď n list = (head list: prořeď n (drop n list))



filtrujVrchy :: [Vrch] -> [Vrch]
filtrujVrchy = prořeď 1


vyrobSvg  :: FilePath -> FilePath -> IO ()
vyrobSvg file2Vrcholy file4vrchSvg = do
    setLocaleEncoding utf8
    putStrLn $ "Prevod do SVG vrcholu: " ++  file2Vrcholy
    text <- readFile file2Vrcholy
    let vrchyVse = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu celkem:         " ++  (show . length) vrchyVse
    let vrchy = filtrujVrchy vrchyVse
    putStrLn $ "Pocet vrchu filtrovano:     " ++  (show . length) vrchy
    createDirectoryIfMissing True (takeDirectory file4vrchSvg)
    writeFile file4vrchSvg  (vrchSvg vrchy)
    
