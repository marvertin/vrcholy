module Konst
    (
      ggdir0, ggdir1, ggfile2, ggdir3, ggfile4
    ) where

import System.FilePath.Posix

dirRoot = "m:/vrch-CZ"

ggdir0 = dirRoot </> "0srtm"     -- Primární SRTM3 data
ggdir1 = dirRoot </> "1srtmByElevation"     -- Transponobvaná SRTM-3 data
ggfile2 = dirRoot </> "2vrcholy.txt"  -- Spočítané vrcholy
ggdir3 = dirRoot </> "3geonames"  -- Geonames
ggfile4 = dirRoot </> "4gpx.gpx"       -- Gpx soubory

