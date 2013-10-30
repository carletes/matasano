-- 12. Byte-at-a-time ECB decryption, Full control version
--
-- Copy your oracle function to a new function that encrypts buffers
-- under ECB mode using a consistent but unknown key (for instance,
-- assign a single random key, once, to a global variable).
--
-- Now take that same function and have it append to the plaintext,
-- BEFORE ENCRYPTING, the following string:
--
--   Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
--   aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
--   dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
--   YnkK
--
-- SPOILER ALERT: DO NOT DECODE THIS STRING NOW. DON'T DO IT.
--
-- Base64 decode the string before appending it. DO NOT BASE64 DECODE THE
-- STRING BY HAND; MAKE YOUR CODE DO IT. The point is that you don't know
-- its contents.
--
-- What you have now is a function that produces:
--
--   AES-128-ECB(your-string || unknown-string, random-key)
--
-- You can decrypt "unknown-string" with repeated calls to the oracle
-- function!
--
-- Here's roughly how:
--
-- a. Feed identical bytes of your-string to the function 1 at a time ---
-- start with 1 byte ("A"), then "AA", then "AAA" and so on. Discover the
-- block size of the cipher. You know it, but do this step anyway.
--
-- b. Detect that the function is using ECB. You already know, but do
-- this step anyways.
--
-- c. Knowing the block size, craft an input block that is exactly 1 byte
-- short (for instance, if the block size is 8 bytes, make
-- "AAAAAAA"). Think about what the oracle function is going to put in
-- that last byte position.
--
-- d. Make a dictionary of every possible last byte by feeding different
-- strings to the oracle; for instance, "AAAAAAAA", "AAAAAAAB",
-- "AAAAAAAC", remembering the first block of each invocation.
--
-- e. Match the output of the one-byte-short input to one of the entries
-- in your dictionary. You've now discovered the first byte of
-- unknown-string.
--
-- f. Repeat for the next byte.

import Data.Word (Word8)
import Data.Maybe (mapMaybe)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map

import qualified Matasano as M

unknownString :: String
unknownString = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbX" ++
                "kgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmlu" ++
                "ZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZH" ++
                "JvdmUgYnkK"

oracle          :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
oracle bs unk k = M.encryptAES_ECB_PKCS7 k (B.concat [bs, unk])

detectECB    :: B.ByteString -> Integer
detectECB bs = case chunkSizes of
                 [] -> 0
                 _  -> maximum chunkSizes
    where
      chunkSizes = map (fromIntegral . M.longuestChunk) freqs
      freqs      = mapMaybe (flip M.detectECB bs . fromIntegral) [n, n-1 .. 1]
      n          = B.length bs `div` 2

type BlockMap = Map.Map B.ByteString Word8

mkBlockMap :: B.ByteString -> B.ByteString -> Integer -> BlockMap
mkBlockMap unk k n = foldl go Map.empty [0 .. 255] where
    go     :: BlockMap -> Word8 -> BlockMap
    go m w = Map.insert block' w m where
        block' = oracle block unk k
        block  = B.concat [B.replicate (fromIntegral (n - 1)) 0, B.singleton w]

findByte         :: B.ByteString -> B.ByteString -> Integer -> Maybe Word8
findByte unk k n = Map.lookup block' blockMap where
    block'   = oracle block unk k
    block    = B.concat [prefix, B.take 1 unk]
    prefix   = B.replicate (fromIntegral (n - 1)) 0
    blockMap = mkBlockMap unk k n

-- Guess ECB block size of @unknown@ (without looking at the length of @k@).
--
-- Sometimes @detectECB@ reports a block size of 2 or 3 for some of the first
-- iterations, so we say here:
--
--     dropWhile (< 4)
--
-- instead of the obvious:
--
--     dropwhile (< 2)
blockSize           :: B.ByteString -> B.ByteString -> Integer
blockSize unknown k = head $ dropWhile (< 3) $ map process [1 ..] where
    process   :: Int -> Integer
    process n = detectECB bs where
        bs :: B.ByteString
        bs = oracle (C.replicate (fromIntegral n) 'A') unknown k

solve          :: B.ByteString -> B.ByteString -> IO ()
solve unknown k = do
  let blk = blockSize unknown k
      bytes = findByte unknown k blk
  print bytes

main :: IO ()
main = do
    k <- M.randomAESKey
    let unknown = M.base64ToBytes unknownString
    case unknown of
      Left err -> do
                   putStrLn $ "Invalid base64-encoded text: " ++ err
                   exitWith $ ExitFailure 1
      Right bs -> solve bs k
