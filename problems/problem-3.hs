--- 3. Single-character XOR Cipher
---
--- The hex encoded string:
---
---   1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
---
--- has been XOR'd against a single character. Find the key, decrypt
--- the message.
---
--- Write code to do this for you. How? Devise some method for "scoring" a
--- piece of English plaintext. (Character frequency is a good metric.)
--- Evaluate each output and choose the one with the best score.

import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.List (sort)
import System.Environment (getArgs)

import qualified Matasano as M
import Matasano.Utils (usage)

candidates = do
  input <- M.hexToBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let keys = map (\w -> (w, B.replicate inputLength w)) [1, 2 .. 255] where
          inputLength = B.length input
  return (map (\(k, k') -> (k, M.xorEncrypt input k')) keys)

rankCandidates       :: [(Word8, B.ByteString)] -> M.Frequencies -> [(Double, Word8, B.ByteString)]
rankCandidates cs f = map (\(w, bs) -> (M.rank bs f, w, bs)) cs

main = do
  argv <- getArgs
  case argv of
    [corpus] -> do
              freqs <- M.corpusFrequencies corpus
              let result = case candidates of
                             Left err  -> "Error: " ++ err
                             Right bbs -> show $ minimum (rankCandidates bbs freqs)
              putStrLn result
    _ -> usage "<corpus file>"
