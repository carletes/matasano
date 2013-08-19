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
    , frequencies
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

-- For 'xor'
import Data.Bits

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

-- | Return a 'Data.Map' of the frequences of each byte in a byte string
frequencies    :: B.ByteString -> Map.Map Char Integer
frequencies bs =  foldl process Map.empty (C.unpack bs) where
    process :: Map.Map Char Integer -> Char -> Map.Map Char Integer
    process map c = case Map.lookup c map of
                      Nothing -> Map.insert c 1 map
                      Just n  -> Map.insert c (n + 1) map
