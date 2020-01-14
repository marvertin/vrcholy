module Konst
    (
      dirRoot, dir1srtm, dir2srtm, dir3vrcholy, dir4gpx
    ) where

import System.FilePath.Posix

dirRoot = "g:/vrch"

dir1srtm = dirRoot </> "1srtm"
dir2srtm = dirRoot </> "2srtm"
dir3vrcholy = dirRoot </> "3vrch"
dir4gpx = dirRoot </> "4gpx"