{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module CmdLine
    (   
      dirRoot
    ) where

import System.Directory    
import System.Environment

dirRoot :: IO FilePath
dirRoot = do
  args <- getArgs
  case args of
    [] -> error "Program must have one argument which is root directory of processed data."
    [rootDir] -> return rootDir
    _ -> error "Program must have ONLY one argument which is root directory of processed data."
      
-- return "m:/vrch-CZ"