-- | Helper functions for solving the Matasano Crypto Challenges:
-- <http://www.matasano.com/articles/crypto-challenges/>
module Matasano
    ( -- * Hexadecimal conversion functions
      hexToBytes
    , bytesToHex
      -- * Base-64 conversion functions
    , base64ToBytes
    , bytesToBase64
    ) where

import qualified Data.ByteString.Lazy as B

-- | Return a byte string from its hex string representaion
hexToBytes    :: String -> B.ByteString
hexToBytes = error "XXX Unimplemented"

-- | Return the hex representation of a byte string
bytesToHex    :: B.ByteString -> String
bytesToHex = error "XXX Unimplemented"

-- | Return a byte string from its base-64 representation
base64ToBytes    :: String -> B.ByteString
base64ToBytes = error "XXX Unimplemented"

-- | Return the base-64 representation of a byte string
bytesToBase64 :: B.ByteString -> String
bytesToBase64 = error "XXX Unimplemented"
