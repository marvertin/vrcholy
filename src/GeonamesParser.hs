{-# LANGUAGE OverloadedStrings #-}

module GeonamesParser
    (  
        dejNazev
    ) 
    where

import GeonamesIo

import Data.Aeson.Lens 
import Control.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

-- ústřední funkcezodpovědná za vrácení toho sptávného názv
--  dispozici má informaci zda je to vrcjol nebo sedlo a celý json stažený z geonames
dejNazev = dejToponymNameZPrvnihoZaznamu

dejToponymNameZPrvnihoZaznamu :: Kotyp -> B.ByteString -> Maybe String
dejToponymNameZPrvnihoZaznamu kotyp jso = fmap T.unpack $ jso ^? (key "geonames" . nth 0 . key "toponymName" . _String)
