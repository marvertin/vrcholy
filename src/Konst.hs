module Konst
    (
      ggdir0, ggdir1, ggfile2, ggdir3, ggfile4gpx, ggfile4vrchSvg
    ) where

import CmdLine
import System.FilePath.Posix

ggdir0 = fmap (</> "0srtm") dirRoot     -- Primární SRTM3 data
ggdir1 = fmap (</> "1srtmByElevation") dirRoot    -- Transponobvaná SRTM-3 data
ggfile2 = fmap (</> "2vrcholy.txt") dirRoot  -- Spočítané vrcholy
ggdir3 = fmap (</> "3geonames") dirRoot  -- Geonames
ggfile4gpx = fmap (</> "4gpx.gpx") dirRoot       -- Gpx soubory
ggfile4vrchSvg = fmap (</> "4vrch.svg") dirRoot       -- Gpx soubory

