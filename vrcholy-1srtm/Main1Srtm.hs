module Main where

import PripravaVstupu
import Konst

import Control.Monad

main :: IO ()
main = join $ liftM2 transponujSrtm ggdir0 ggdir1


