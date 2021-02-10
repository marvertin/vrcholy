{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Nacitac
import ZapisovacVrchSvg
import Hledac
import Potopa
import Uzemi
import VrchTypy
import PripravaVstupu
import Konst
import GeonamesParser
import JsonParser
import GeonamesIo
import PrQuadTree

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
main = join $ liftM2 vyrobGpx ggfile2 ggfile4vrchSvg

vyrobGpx  :: FilePath -> FilePath -> IO ()
vyrobGpx file2Vrcholy file4vrchSvg = do
    setLocaleEncoding utf8
    putStrLn $ "Prevod do SVG vrcholu: " ++  file2Vrcholy
    text <- readFile file2Vrcholy
    let vrchy = map read (lines text) :: [Vrch]
    putStrLn $ "Pocet vrchu:     " ++  (show . length) vrchy
    createDirectoryIfMissing True (takeDirectory file4vrchSvg)
    writeFile file4vrchSvg  (vrchSvg vrchy)
    
