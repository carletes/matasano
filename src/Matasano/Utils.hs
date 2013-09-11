-- | Helper functions
module Matasano.Utils
    (
     usage
    ) where

import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode(..))

usage      :: String -> IO ()
usage args = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " " ++ args
  exitWith $ ExitFailure 1