module Main where

import Lib
import Nacitac
import Zapisovac
import PotopaNova
import Uzemi
import VrchTypy
import PripravaVstupu

import System.FilePath.Posix
import Konst
import System.Directory
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad

minimalniProminence = 10

main = join $ liftM2 potopaSvetaZTransponovanychSrtm ggdir1 ggfile2

potopaSvetaZTransponovanychSrtm :: FilePath -> FilePath -> IO ()
potopaSvetaZTransponovanychSrtm dirInput fileOutput = do
    putStrLn $ "Potopa sveta primo z transponovanych srtm"
    hladiny <- loadAll dirInput
    let vrcholy = potopaSveta minimalniProminence hladiny
    createDirectoryIfMissing True (takeDirectory fileOutput)
    writeFile fileOutput $ unlines $ map show vrcholy
    putStrLn "Konec potopy, voda opadla."

