--- 6. Break repeating-key XOR
---
--- The buffer at the following location:
---
---   https://gist.github.com/3132752
---
--- is base64-encoded repeating-key XOR. Break it.
---
--- Here's how:
---
--- a. Let KEYSIZE be the guessed length of the key; try values from 2 to
--- (say) 40.
---
--- b. Write a function to compute the edit distance/Hamming distance
--- between two strings. The Hamming distance is just the number of
--- differing bits. The distance between:
---
---   this is a test
---
--- and:
---
---   wokka wokka!!!
---
--- is 37.
---
--- c. For each KEYSIZE, take the FIRST KEYSIZE worth of bytes, and the
--- SECOND KEYSIZE worth of bytes, and find the edit distance between
--- them. Normalize this result by dividing by KEYSIZE.
---
--- d. The KEYSIZE with the smallest normalized edit distance is probably
--- the key. You could proceed perhaps with the smallest 2-3 KEYSIZE
--- values. Or take 4 KEYSIZE blocks instead of 2 and average the
--- distances.
---
--- e. Now that you probably know the KEYSIZE: break the ciphertext into
--- blocks of KEYSIZE length.
---
--- f. Now transpose the blocks: make a block that is the first byte of
--- every block, and a block that is the second byte of every block, and
--- so on.
---
--- g. Solve each block as if it was single-character XOR. You already
--- have code to do this.
---
--- e. For each block, the single-byte XOR key that produces the best
--- looking histogram is the repeating-key XOR key byte for that
--- block. Put them together and you have the key.

import Data.List (genericLength, sort)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Matasano as M

rankedKeySizes           :: B.ByteString -> Integer -> [(Double, Integer)]
rankedKeySizes bs maxLen =  sort $ map process [1 .. maxLen] where
    process   :: Integer -> (Double, Integer)
    process n = (dNorm, n) where
        dNorm      = (average $ distances pairs') / (fromIntegral n)
        distances  = map (\(blk1, blk2) -> M.hammingDistance blk1 blk2)
        pairs'     = take 10000 $ pairs $ chunks n bs
        average xs = realToFrac (sum xs) / genericLength xs

chunks      :: Integer -> B.ByteString -> [B.ByteString]
chunks n xs = if xs == B.empty
              then []
              else first : chunks n rest where
                  (first, rest) = B.splitAt (fromIntegral n) xs

pairs        :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = (map (pair x) xs) ++ pairs xs where
    pair     :: a -> a -> (a, a)
    pair u v = (u, v)

solve :: B.ByteString -> M.Frequencies -> Integer -> [[M.RankedKey]]
solve bs freqs keySize = map (\bs -> M.guessXorKey bs freqs 0.5) bss' where
    bss' :: [B.ByteString]
    bss' = B.transpose $ take (fromIntegral keySize) $ chunks keySize bs

main :: IO ()
main = do
  [dataFile, corpusFile] <- getArgs
  freqs <- M.corpusFrequencies corpusFile
  input <- readFile dataFile
  case ((M.base64ToBytes . concat . lines) input) of
    Left err     -> do
                   putStrLn $ "Error: " ++ err
                   exitWith $ ExitFailure 1
    Right input' -> do
               let keySizes = rankedKeySizes input' 40
                   result = map (solve input' freqs . snd) keySizes
               putStrLn $ show result
