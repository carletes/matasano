--- 4. Detect single-character XOR
---
--- One of the 60-character strings at:
---
---   https://gist.github.com/3132713
---
--- has been encrypted by single-character XOR. Find it. (Your code from
--- #3 should help.)

import System.Environment
import qualified Data.ByteString.Lazy as B

import qualified Matasano as M

getStrings       :: FilePath -> IO [Either String B.ByteString]
getStrings fname = do
  body <- readFile fname
  return (map M.hexToBytes (lines body))

candidates         :: [Either String B.ByteString] -> M.Frequencies -> Double -> [Either String [M.RankedKey]]
candidates bbs f r = map process bbs where
    process    :: Either String B.ByteString -> Either String [M.RankedKey]
    process bs = case bs of
                   Left err  -> Left err
                   Right val -> Right (M.guessXorKey val f r)
main :: IO ()
main = do
  [dataFile, corpusFile] <- getArgs
  freqs <- M.corpusFrequencies corpusFile
  input <- getStrings dataFile
  let probable  = filter nonEmpty $ candidates input freqs threshold
      threshold = 0.5
      nonEmpty                :: Either String [M.RankedKey] -> Bool
      nonEmpty (Right (x:xs)) = True
      nonEmpty _              = False
  putStrLn $ show probable
