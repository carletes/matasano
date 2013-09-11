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
    , corpusFrequencies
    , frequencies
    , rank
    , hammingDistance

      -- * Cryptanalysis functions
    , RankedKey
    , guessXorKey
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
import Data.Bits (popCount, xor)
import Data.List (sort)
import Data.Word (Word8)

-- | Return a byte string from its hex string representation.
hexToBytes :: String -> Either String B.ByteString
hexToBytes hs = let (decoded, err) = B16.decode $ C.pack hs
                    errorStr = C.unpack err in
                case errorStr of
                  "" -> Right decoded
                  _  -> Left $ "Invalid hex string: " ++ errorStr

-- | Return the hex representation of a byte string.
bytesToHex :: B.ByteString -> String
bytesToHex =  C.unpack . B16.encode

-- | Return a byte string from its base-64 representation.
base64ToBytes    :: String -> Either String B.ByteString
base64ToBytes bs = let bs' = B64.decode $ C.pack bs in
                   case bs' of
                     Left err -> Left $ "Invalid base64 string: " ++ err
                     Right _  -> bs'

-- | Return the base-64 representation of a byte string.
bytesToBase64 :: B.ByteString -> String
bytesToBase64 = C.unpack . B64.encode

-- | Return the ASCII encoding of a byte string.
bytesToASCII :: B.ByteString -> String
bytesToASCII = C.unpack

-- | Encrypts a byte string by xor'ing with another one.
xorEncrypt     :: B.ByteString -> B.ByteString -> B.ByteString
xorEncrypt a b = B.pack $ zipWith (xor) (B.unpack a) (B.unpack b)

-- | A structure describing the frequencies of bytes in a byte string.
data Frequencies = Freqs
    { freqs :: Map.Map Char Integer  -- ^ The frequency map
    , norm  :: Double                -- ^ Length of the byte string
    }

-- | Return the frequency map of each byte in a byte string.
frequencies    :: B.ByteString -> Frequencies
frequencies bs =  Freqs freqMap norm' where
    freqMap = foldl process Map.empty (C.unpack bs)
    norm'   = fromIntegral $ B.length bs
    process :: Map.Map Char Integer -> Char -> Map.Map Char Integer
    process m c = case Map.lookup c m of
                    Nothing -> Map.insert c 1 m
                    Just n  -> Map.insert c (n + 1) m

-- | Return the frequency of a given byte in a frequency map.
frequency     :: Char -> Frequencies -> Double
frequency c f = case Map.lookup c (freqs f) of
                  Nothing -> 0.0
                  Just n  -> (fromIntegral n) / norm f

-- | Compute how much a byte string diverges from a frequency map.
-- Returns 0.0 for a perfect fit, and 1.0 for a complete miss
rank      :: B.ByteString -> Frequencies -> Double
rank bs f = sum $ map freqDelta bsChars where
                freqDelta c = abs (frequency c f - frequency c freqBs)
                freqBs      = frequencies bs
                bsChars     = Map.keys (freqs freqBs)

-- | Return the character frequencies of a given corpus file.
corpusFrequencies       :: FilePath -> IO Frequencies
corpusFrequencies fname =  do
  corpus <- B.readFile fname
  return (frequencies corpus)

-- | A key ranked according to some criteria.
data RankedKey = RankedKey
    { key       :: Word8         -- ^ The key
    , rnk       :: Double        -- ^ The rank of this key
    , decrypted :: B.ByteString  -- ^ The byte string decrypted with this key
} deriving (Show, Eq)

instance Ord RankedKey where
    compare k1 k2 = compare (rnk k1) (rnk k2)

-- | Guess the single-byte XOR key used to encrypt the given byte string.
--
-- Given a byte string, a frequencies map and a threshold, returns a list of
-- ranked keys whose rank (for the given frequencies map) is bellow the given
-- threshold.
--
-- The returned keys are sorted by rank (lowest first)
guessXorKey        :: B.ByteString -> Frequencies -> Double -> [RankedKey]
guessXorKey bs f t = sort $ filter (\k -> rnk k < t) candidates where
    candidates = map go [1 .. 225]
    go   :: Word8 -> RankedKey
    go k =  RankedKey k r d where
        r   = rank d f
        d   = xorEncrypt bs (B.replicate len k)
        len = B.length bs

-- | Return the number of differing bits in two byte strings.
hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance bs cs = sum $ map (\(a, b) -> popCount $ xor a b) (B.zip bs cs)
