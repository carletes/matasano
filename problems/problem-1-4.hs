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
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Matasano as M

getStrings       :: FilePath -> IO [B.ByteString]
getStrings fname = do
  body <- B.readFile fname
  return (C.split '\n' body)

decryptedStrings      :: B.ByteString -> M.Frequencies -> [(Double, Word8, B.ByteString)]
decryptedStrings bs f = map process keys where
    keys = [1 .. 255]
    process :: Word8 -> (Double, Word8, B.ByteString)
    process k = (r, k, decrypted) where
        decrypted = M.xorEncrypt bs (B.pack [k])
        r         = M.rank decrypted f

main = do
  [dataFile, corpusFile] <- getArgs
  freqs <- M.corpusFrequencies corpusFile
  result <- getStrings dataFile
  putStrLn $ show result
