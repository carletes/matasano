-- | Helper functions for solving the Matasano Crypto Challenges:
-- <http://www.matasano.com/articles/crypto-challenges/>
module Matasano
    ( -- * Hexadecimal conversion functions
      hexToBytes
    , bytesToHex
      -- * Base-64 conversion functions
    , base64ToBytes
    , bytesToBase64
      -- * ASCII conversion functions
    , bytesToASCII
      -- * Encryption functions
    , xorEncrypt
      -- * Text analysis functions
    , Frequencies
    , frequencies
    , rank
    ) where

-- Looks like the best way to handle raw byte data in Haskell is with
-- the 'Data.ByteString' module.
--
-- I'll use the lazy version here, since I might need this for data that
-- does not fit into memory.
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base64.Lazy as B64

import qualified Data.Map as Map
import Data.Bits (xor)
import Data.Int (Int64)

-- | Return a byte string from its hex string representaion
hexToBytes :: String -> Either String B.ByteString
hexToBytes hs = let (decoded, error) = B16.decode $ C.pack hs
                    errorStr = C.unpack error in
                case errorStr of
                  "" -> Right decoded
                  otherwise -> Left $ "Invalid hex string: " ++ errorStr

-- | Return the hex representation of a byte string
bytesToHex :: B.ByteString -> String
bytesToHex =  C.unpack . B16.encode

-- | Return a byte string from its base-64 representation
base64ToBytes    :: String -> Either String B.ByteString
base64ToBytes bs = let bs' = B64.decode $ C.pack bs in
                   case bs' of
                     Left err -> Left $ "Invalid bas64 string: " ++ err
                     Right _  -> bs'

-- | Return the base-64 representation of a byte string
bytesToBase64 :: B.ByteString -> String
bytesToBase64 = C.unpack . B64.encode

-- | Return the ASCII encoding of a byte string
bytesToASCII = C.unpack

-- | Encrypts a byte string by xor'ing with another one
xorEncrypt     :: B.ByteString -> B.ByteString -> B.ByteString
xorEncrypt a b = B.pack $ zipWith (xor) (B.unpack a) (B.unpack b)

data Frequencies = Freqs {
      freqs :: Map.Map Char Integer,
      count :: Int64
}

-- | Return the frequency map of each byte in a byte string
frequencies    :: B.ByteString -> Frequencies
frequencies bs =  Freqs freqMap count where
    freqMap = foldl process Map.empty (C.unpack bs)
    count = B.length bs
    process :: Map.Map Char Integer -> Char -> Map.Map Char Integer
    process map c = case Map.lookup c map of
                      Nothing -> Map.insert c 1 map
                      Just n  -> Map.insert c (n + 1) map

-- | Return the frequency of a given byte in a frequency map
frequency     :: Char -> Frequencies -> Double
frequency c f = case Map.lookup c (freqs f) of
                  Nothing -> 0.0
                  Just n  -> (fromIntegral n) / fromIntegral (count f)

-- | Compute how much a byte string diverges from a frequency map.
-- Returns 0.0 for a perfect fit, and 1.0 for a complete miss
rank      :: B.ByteString -> Frequencies -> Double
rank bs f = sum $ map freqDelta bsChars where
                freqDelta c = abs (frequency c f - frequency c freqBs)
                freqBs      = frequencies bs
                bsChars     = Map.keys (freqs freqBs)
