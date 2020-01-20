module Konst
    (
      dirRoot, dir1srtm, dir2srtm, dir3vrcholy, dir4geonames, dir5gpx
    ) where

import System.FilePath.Posix

dirRoot = "m:/vrch"

dir1srtm = dirRoot </> "1srtm"     -- Primární SRTM3 data
dir2srtm = dirRoot </> "2srtm"     -- Transponobvaná SRTM-3 data
dir3vrcholy = dirRoot </> "3vrch"  -- Spočítané vrcholy
dir4geonames = dirRoot </> "3geonames"  -- Geonames
dir5gpx = dirRoot </> "5gpx"       -- Gpx soubory