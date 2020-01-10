
module PripravaVstupu
  ( 
    transponujSrtm,
    ) where

import Nacitac
    
import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

import System.Directory
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M

import System.FilePath.Posix
import  Control.Arrow
import  Control.Monad

-- Pro řešení potopy je vhodné mít data uspořádána podle výšek
transponujSrtm :: FilePath -> FilePath -> IO ()
transponujSrtm srcDir destDir = do
    putStrLn $ "Transpozice srtm dat."
    soubory <- listDirectory srcDir
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory

    -- obsahy <- mapM B.readFile $ 
    forM_ (map (srcDir </>) soubory) $ \fileName -> do
        transponujFile fileName destDir

--        let souradky = map fileNameToCoord soubory 

    --let body = load (zip souradky obsahy)

transponujFile :: FilePath -> FilePath -> IO ()
transponujFile fileName destDir = do
    print fileName
