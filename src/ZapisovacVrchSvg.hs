{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module ZapisovacVrchSvg
    (   vrchSvg
    ) where
 
import Lib
import Uzemi
import Gps
import Control.Arrow
import VrchTypy(Vrch(..), Kopec(..))
import Data.String.Interpolate ( i )
import Text.Printf (printf)
import Data.List
import Data.Function
import Data.Maybe

-- 49.2839519N, 16.3563408E
vrchSvg :: [Vrch] -> String
vrchSvg vrchyp = "tady to bude"