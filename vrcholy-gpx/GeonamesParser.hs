{-# LANGUAGE OverloadedStrings #-}

module GeonamesParser
    (  
        q
    ) 

    where

import Data.Aeson.Lens 
import Control.Lens
import qualified Data.Text as T
-- import qualified Data.Text as T

jso = "{\"geonames\":[{\"adminCode1\":\"80\",\"lng\":\"16.05969\",\"distance\":\"0.35187\",\"geonameId\":9178347,\"toponymName\":\"(Such\195\189 kopec [Sn\196\155\197\190n\195\169])\",\"countryId\":\"3077311\",\"fcl\":\"T\",\"population\":0,\"countryCode\":\"CZ\",\"name\":\"(Such\195\189 kopec [Sn\196\155\197\190n\195\169])\",\"fclName\":\"mountain,hill,rock,... \",\"adminCodes1\":{\"ISO3166_2\":\"63\"},\"countryName\":\"Czechia\",\"fcodeName\":\"hill\",\"adminName1\":\"Vyso\196\141ina\",\"lat\":\"49.66425\",\"fcode\":\"HLL\"},{\"adminCode1\":\"80\",\"lng\":\"16.0714\",\"distance\":\"0.64682\",\"geonameId\":9178346,\"toponymName\":\"Mal\195\173nsk\195\161 sk\195\161la [Sn\196\155\197\190n\195\169]\",\"countryId\":\"3077311\",\"fcl\":\"T\",\"population\":0,\"countryCode\":\"CZ\",\"name\":\"Mal\195\173nsk\195\161 sk\195\161la [Sn\196\155\197\190n\195\169]\",\"fclName\":\"mountain,hill,rock,... \",\"adminCodes1\":{\"ISO3166_2\":\"63\"},\"countryName\":\"Czechia\",\"fcodeName\":\"rock\",\"adminName1\":\"Vyso\196\141ina\",\"lat\":\"49.66101\",\"fcode\":\"RK\"},{\"adminCode1\":\"80\",\"lng\":\"16.05286\",\"distance\":\"0.87372\",\"geonameId\":9881690,\"toponymName\":\"K\197\153ovina [Sn\196\155\197\190n\195\169-Samot\195\173n]\",\"countryId\":\"3077311\",\"fcl\":\"T\",\"population\":0,\"countryCode\":\"CZ\",\"name\":\"K\197\153ovina [Sn\196\155\197\190n\195\169-Samot\195\173n]\",\"fclName\":\"mountain,hill,rock,... \",\"adminCodes1\":{\"ISO3166_2\":\"63\"},\"countryName\":\"Czechia\",\"fcodeName\":\"hill\",\"adminName1\":\"Vyso\196\141ina\",\"lat\":\"49.65692\",\"fcode\":\"HLL\"}]}" :: String
    
q :: IO ()
q = do
    -- print $ "[10]" ^? nth 0 . _Integral
    let jsn = "{\"a\": \"xyz\", \"b\": null}" :: String
    print ( jsn ^? key "a" . nonNull )
    let (Just menko) =  jso ^? (key "geonames" . nth 0 . key "toponymName" . _String)
    let mm = T.unpack menko :: String
    print menko
    putStrLn mm
    -- print $ "{\"a\": 100, \"b\": 200}" ^? key "a"
        
