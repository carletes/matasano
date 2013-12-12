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

type Oracle a = M.Oracle12 a

-- | Detect the ECB block size of a give byte string.
detectECB :: B.ByteString  -- ^ Input byte string
          -> Integer       -- ^ Block size
detectECB bs = case chunkSizes of
                 [] -> 0
                 _  -> maximum chunkSizes
    where
      chunkSizes = map (fromIntegral . M.longuestChunk) freqs
      freqs      = mapMaybe (flip M.detectECB bs . fromIntegral) [n, n-1 .. 1]
      n          = B.length bs `div` 2

type BlockMap = Map.Map B.ByteString Word8

oracle :: B.ByteString -> Oracle B.ByteString
oracle = M.oracle12

-- | Build the block map needed to find the next unknown byte of a block.
mkBlockMap :: Integer          -- ^ Block size
           -> B.ByteString     -- ^ Bytes found in this block
           -> B.ByteString     -- ^ Bytes found in previous blocks
           -> Oracle BlockMap  -- ^ The block map
mkBlockMap n known b = foldM go Map.empty [0 .. 255] where
    go     :: BlockMap -> Word8 -> Oracle BlockMap
    go m w = do
      let block = B.concat [prefix,
                            known,
                            B.singleton w]
          prefix = if b == B.empty
                   then B.replicate (fromIntegral (n - (k' + 1))) 0
                   else B.drop (k + 1) b
          k      = B.length known
          k'     = fromIntegral k
          n'     = fromIntegral n
      enc <- oracle block
      let block' = B.take n' $ B.drop (B.length b - n') enc
      return $ Map.insert block' w m

-- | Find the next byte in the first block.
nextByte :: Integer                      -- ^ Block size
         -> Maybe B.ByteString           -- ^ Bytes found in this block
         -> Integer                      -- ^ Dummy parameter
         -> Oracle (Maybe B.ByteString)  -- ^ New block
nextByte _ Nothing _      = return Nothing
nextByte n (Just known) _ = do
  --blockMap <- mkBlockMap n known
  blockMap <- mkBlockMap n known B.empty
  let block  = B.replicate (fromIntegral (n - (k + 1))) 0
      k      = fromIntegral $ B.length known
  block' <- oracle block
  return $ case Map.lookup (B.take (fromIntegral n) block') blockMap of
             Just b' -> Just $ B.concat [known, B.singleton b']
             Nothing -> Nothing

-- | Find the first block of the oracle's secret.
firstBlock :: Integer                     -- ^ Block size
          -> Oracle (Maybe B.ByteString)  -- ^ The first block
firstBlock n = do
  bytes <- foldM (nextByte n) (Just B.empty) [1 .. n]
  case sequence [bytes] of
    Nothing -> return Nothing
    Just bs -> return $ Just (B.concat bs)

-- | Find the next byte in a block.
nextByte' :: Integer                      -- ^ Block size
          -> Maybe B.ByteString           -- ^ Bytes found in previous blocks
          -> Maybe B.ByteString           -- ^ Bytes found in this block
          -> Integer                      -- ^ Dummy parameter
          -> Oracle (Maybe B.ByteString)  -- ^ New block
nextByte' _ _ Nothing _             = return Nothing
nextByte' n (Just b) (Just known) _ = do
  blockMap <- mkBlockMap n known b
  let block = B.concat [b,
                        B.replicate (n' - (k + 1)) 0]
      k     = fromIntegral $ B.length known
      n'    = fromIntegral n
  enc <- oracle block
  let block' = B.take n' $ B.drop (2 * B.length b) enc
  return $ case Map.lookup block' blockMap of
             Just b' -> Just $ B.concat [known, B.singleton b']
             Nothing -> Just known

-- | Find the next bytes in a block.
nBlocks :: Integer                      -- ^ Block size
        -> Maybe B.ByteString           -- ^ Bytes found so far
        -> Oracle (Maybe B.ByteString)  -- ^ New block
nBlocks n b = do
  bytes <- foldM (nextByte' n b) (Just B.empty) [1 .. n]
  case sequence (b : [bytes]) of
    Nothing -> return Nothing
    Just bs -> return $ Just (B.concat bs)

-- | Guess ECB block size of the oracle function.
--
-- Sometimes @detectECB@ reports a block size of 2 or 3 for some of the first
-- iterations, so we say here:
--
--     dropWhile (< 4)
--
-- instead of the obvious:
--
--     dropwhile (< 2)
blockSize :: Oracle Integer
blockSize = do
  let go   :: Int -> Oracle Integer
      go n = do
              bs <- oracle (B.replicate (fromIntegral n) 0)
              return $ detectECB bs
  candidates <- forM [1 ..] go
  return $ head $ dropWhile (< 4) candidates

solve :: Oracle (Maybe B.ByteString)
solve = do
  len   <- blockSize
  enc <- oracle B.empty
  b1 <- firstBlock len
  let count = fromIntegral (B.length enc) `div` len
  foldM (\b _ -> nBlocks len b) b1 [2 .. count]

main :: IO ()
main = do
  env <- M.mkOracle12Env
  let secret = M.runOracle12 solve env
  print secret
