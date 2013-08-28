--- 4. Detect single-character XOR
---
--- One of the 60-character strings at:
---
---   https://gist.github.com/3132713
---
--- has been encrypted by single-character XOR. Find it. (Your code from
--- #3 should help.)

import Data.Word
import System.Environment
import qualified Data.ByteString.Lazy as B

import qualified Matasano as M

getStrings       :: FilePath -> IO [Either String B.ByteString]
getStrings fname = do
  body <- readFile fname
  return (map M.hexToBytes (lines body))

data RankedKey = RankedKey {
      key       :: Word8,
      rank      :: Double,
      decrypted :: B.ByteString
}

data Candidate = Candidate {
      original :: B.ByteString,
      keys     :: [RankedKey]
}

candidates :: [Either String B.ByteString] -> M.Frequencies -> [Either String Candidate]
candidates bbs f = map process bbs where
    process    :: Either String B.ByteString -> Either String Candidate
    process bs = case bs of
                   Left err  -> Left err
                   Right val -> Right (Candidate val (decryptRank val f))

decryptRank      :: B.ByteString -> M.Frequencies -> [RankedKey]
decryptRank bs f = map process ks where
    ks = [1 .. 255]
    process   :: Word8 -> RankedKey
    process k = RankedKey k r d where
        r   = M.rank d f
        d   = M.xorEncrypt bs (B.replicate len k)
        len = B.length bs

printCandidate              :: Double -> Either String Candidate -> IO ()
printCandidate _ (Left err) = putStrLn err
printCandidate r (Right c)  = do
  putStrLn $ M.bytesToHex (original c)
  mapM_ printKey ks where
      printKey   :: RankedKey -> IO ()
      printKey k = putStrLn $ "\t" ++ show (rank k) ++ " " ++ show (key k) ++ " " ++ show (decrypted k)
      ks         = filter (\k -> (rank k) < r) (keys c)

bellowRank             :: Double -> Either String Candidate -> Bool
bellowRank _ (Left _)  = False
bellowRank r (Right c) = any (\k -> (rank k) < r) (keys c)

main :: IO ()
main = do
  [dataFile, corpusFile] <- getArgs
  freqs <- M.corpusFrequencies corpusFile
  input <- getStrings dataFile
  let allCandidates = candidates input freqs
      threshold     = 0.5
      probable      = filter (bellowRank threshold) allCandidates
  mapM_ (printCandidate threshold) probable
