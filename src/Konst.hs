module Konst
    (
      ggdir0, ggdir1, ggfile2, ggfile3, ggfile4
    ) where

import System.FilePath.Posix

dirRoot = "m:/vrch-CZ"

ggdir0 = dirRoot </> "0srtm"     -- Primární SRTM3 data
ggdir1 = dirRoot </> "1srtmByElevation"     -- Transponobvaná SRTM-3 data
ggfile2 = dirRoot </> "2vrcholy.txt"  -- Spočítané vrcholy
ggfile3 = dirRoot </> "3geonames.txt"  -- Geonames
ggfile4 = dirRoot </> "4gpx.gpx"       -- Gpx soubory

