module Main where

import PripravaVstupu


main :: IO ()
main = do
    transponujSrtm "m:/vrcholy-data/z-geogetu/vse" 
                   "m:/vrcholy-data/temp-vrcholy-srtm"


