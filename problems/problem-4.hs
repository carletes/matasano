--- 4. Detect single-character XOR
---
--- One of the 60-character strings at:
---
---   https://gist.github.com/3132713
---
--- has been encrypted by single-character XOR. Find it. (Your code from
--- #3 should help.)

import Control.Monad (sequence)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B

import qualified Matasano as M
import Matasano.Utils (usage)

getStrings       :: FilePath -> IO (Either String [B.ByteString])
getStrings fname = do
  body <- readFile fname
  return (sequence $ map M.hexToBytes (lines body))

candidates         :: [B.ByteString] -> M.Frequencies -> Double -> [M.RankedKey]
candidates bbs f r = concatMap (\bs -> M.guessXorKey bs f r) bbs

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [dataFile, corpusFile] -> do
              freqs <- M.corpusFrequencies corpusFile
              input <- getStrings dataFile
              case input of
                Left err -> do
                           putStrLn $ "Error: " ++ err
                           exitWith $ ExitFailure 1
                Right input' -> do
                                let result = candidates input' freqs 0.5
                                putStrLn $ show result
    _ -> usage "<data file> <corpus file>"
