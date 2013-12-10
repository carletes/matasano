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

import Control.Monad (foldM, forM)
import Data.Word (Word8)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import qualified Matasano as M

detectECB    :: B.ByteString -> Integer
detectECB bs = case chunkSizes of
                 [] -> 0
                 _  -> maximum chunkSizes
    where
      chunkSizes = map (fromIntegral . M.longuestChunk) freqs
      freqs      = mapMaybe (flip M.detectECB bs . fromIntegral) [n, n-1 .. 1]
      n          = B.length bs `div` 2

type BlockMap = Map.Map B.ByteString Word8

mkBlockMap         :: Integer -> B.ByteString -> M.Oracle12 BlockMap
mkBlockMap n known = foldM go Map.empty [0 .. 255] where
    go     :: BlockMap -> Word8 -> M.Oracle12 BlockMap
    go m w = do
      let block = B.concat [B.replicate (fromIntegral (n - (k + 1))) 0,
                            known,
                            B.singleton w]
          k     = fromIntegral $ B.length known
      block' <- M.oracle12 block
      return $ Map.insert (B.take (fromIntegral n) block') w m

findByte                  :: Integer -> Maybe B.ByteString -> Integer -> M.Oracle12 (Maybe B.ByteString)
findByte _ Nothing _      = return Nothing
findByte n (Just known) _ = do
  blockMap <- mkBlockMap n known
  let block  = B.replicate (fromIntegral (n - (k + 1))) 0
      k      = fromIntegral $ B.length known
  block' <- M.oracle12 block
  return $ case Map.lookup (B.take (fromIntegral n) block') blockMap of
             Just b' -> Just $ B.concat [known, B.singleton b']
             Nothing -> Nothing

findBytes   :: Integer -> M.Oracle12 (Maybe B.ByteString)
findBytes n = do
  bytes <- foldM (findByte n) (Just B.empty) [1 .. n]
  case sequence [bytes] of
    Nothing -> return Nothing
    Just bs -> return $ Just (B.concat bs)

-- Guess ECB block size of the oracle function.
--
-- Sometimes @detectECB@ reports a block size of 2 or 3 for some of the first
-- iterations, so we say here:
--
--     dropWhile (< 4)
--
-- instead of the obvious:
--
--     dropwhile (< 2)
blockSize :: M.Oracle12 Integer
blockSize = do
  let go   :: Int -> M.Oracle12 Integer
      go n = do
              bs <- M.oracle12 (B.replicate (fromIntegral n) 0)
              return $ detectECB bs
  candidates <- forM [1 ..] go
  return $ head $ dropWhile (< 4) candidates

solve :: M.Oracle12 (Maybe B.ByteString)
solve = do
  blk   <- blockSize
  findBytes blk

main :: IO ()
main = do
  env <- M.mkOracle12Env
  let secret = M.runOracle12 solve env
  print secret
