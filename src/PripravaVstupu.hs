
module PripravaVstupu
  ( 
    transponujSrtm,
    transponuj,
    loadAll
    ) where

import Nacitac
import VrchTypy
import Uzemi

import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

import System.Directory

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath.Posix
import  Control.Arrow
import  Control.Monad

import System.IO.Unsafe (unsafeInterleaveIO)

mapM_lazy :: [IO a] -> IO [a]
mapM_lazy [] = return []
mapM_lazy (x:xs) = do
              r <- x
              rs <- unsafeInterleaveIO (mapM_lazy xs)
              return (r:rs)

              

-- Pro řešení potopy je vhodné mít data uspořádána podle výšek
transponujSrtm :: FilePath -> FilePath -> IO ()
transponujSrtm srcDir destDir = do
    putStrLn $ "Transpozice srtm dat."
    soubory <- listDirectory srcDir
    -- print $ map fileNameToCoord soubory
    putStrLn $ "Pocet souboru:     " ++  (show . length) soubory
    -- obsahy <- mapM B.readFile $ 
    createDirectoryIfMissing True destDir
    forM_ soubory $ \fileName -> do
        obracenec <- transponujFile fileName destDir
        forM_ obracenec $ \ (vyska, mous) -> do
            let jmenoFile = destDir </> vyska2DirName vyska
            --putStrLn jmeno
            appendFile jmenoFile $ (unlines . map show) mous


--        let souradky = map fileNameToCoord soubory 
    --let body = load (zip souradky obsahy)
  where
    transponujFile :: FilePath -> FilePath -> IO [(Mnm, [Mou])]
    transponujFile fileName destDir = do
        putStrLn fileName
        obsah <- B.readFile (srcDir </> fileName)
        let body  = kontrolaPoctuBodu $ loadSrtm (fileNameToCoord fileName, obsah) :: [Bod]
        return ( transponuj body )


-- z seznamu bodů seskupí podle nadmořské výšky a vyrobí seznamy souřadnic
transponuj :: [Bod] -> [(Mnm, [Mou])]
transponuj  = M.toList . M.map sort. transponujToMap

transponujToMap :: [Bod] -> M.Map Mnm [Mou]
transponujToMap = M.fromListWith (++) . map (fmap (:[]) . swap)

kontrolaPoctuBodu :: [Bod] -> [Bod]
kontrolaPoctuBodu body = if length body == 1200 * 1200 then body else error "Počet bodů po oříznutí není 1200 * 1200"


dirName2Vyska :: String -> Mnm
dirName2Vyska = read

vyska2DirName :: Mnm -> String
vyska2DirName = show


----------------------
--- čtení transponovaných dat

loadAll :: FilePath ->  IO [Hladina]
loadAll transpoDir = do
    vysky <- fmap (map dirName2Vyska) $ listDirectory transpoDir
    let vyskys = (reverse.sort) vysky
    mapM_lazy $ map (loadOne transpoDir) vyskys

loadOne :: FilePath -> Mnm -> IO Hladina
loadOne transpoDir mnm = do
    text <- readFile (transpoDir </> vyska2DirName mnm) 
    let mous = map read (lines text) :: [Mou]
    return (mous, mnm)

