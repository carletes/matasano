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

      -- * Operations on byte strings
    , chunks
    , pkcs7Pad
    , pkcs7Unpad

      -- * Encryption functions
    , xorEncrypt
    , decryptAES_CBC
    , encryptAES_CBC
    , decryptAES_ECB
    , encryptAES_ECB

      -- * Text analysis functions
    , Frequencies
    , corpusFrequencies
    , frequencies
    , rank
    , hammingDistance

      -- * Cryptanalysis functions
    , RankedKey(key)
    , ChunkFrequencies
    , guessXorKey
    , randomBytes
    , CipherMode(..)
    , detectECB
    , encryptionOracle
    , leakyEncryptionOracle
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
import Crypto.Cipher.AES (decryptECB, encryptECB, initAES)
import Data.Bits (popCount, xor)
import Data.List (sort)
import Data.Word (Word8)
import System.Random (newStdGen, random, randomR, randoms)

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
xorEncrypt a b = B.pack $ zipWith xor (B.unpack a) (B.unpack b)

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
                  Just n  -> fromIntegral n / norm f

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

-- | Splits a byte string in chunks of a given length.
chunks      :: Integer -> B.ByteString -> [B.ByteString]
chunks n bs = if bs == B.empty
              then []
              else first : chunks n rest where
                  (first, rest) = B.splitAt (fromIntegral n) bs

-- | Pad a given byte string to a multiple of given octets using the PKCS#7
-- process (http://tools.ietf.org/html/rfc5652#section-6.3).
--
-- Raises an error if the given chunk length @k@ is not in the range @[1, 256]@
pkcs7Pad      :: Integer -> B.ByteString -> B.ByteString
pkcs7Pad k bs = if not (k > 0 && k <= 256)
                then error $ "pkcs7Pad: Chunk length " ++ show k ++
                         " not the range [1, 256]"
                else B.append bs pad where
                    pad = B.replicate p (fromIntegral p)
                    p   = k' - (B.length bs `mod` k')
                    k'  = fromIntegral k

-- | Removes padding from a byte string which as been padded to a multiple of
-- given octets using the PKCS#7 process
-- (http://tools.ietf.org/html/rfc5652#section-6.3).
--
-- Raises an error if:
--   * the given chunk length @k@ is not in the range @[1, 256]@, or
--   * the length of the given byte string is not a multiple of @k@, or
--   * the padding bytes of the given byte string are not well-formed
pkcs7Unpad      :: Integer -> B.ByteString -> B.ByteString
pkcs7Unpad k bs = if not (k > 0 && k <= 256)
                  then error $ "pkcs7Unpad: Chunk length " ++ show k ++
                       " not the range [1, 256]"
                  else
                      if not (len `mod` k' == 0 )
                      then error $ "pkcs7Unpad: Input length not multiple of " ++ show k
                      else if pad /= expectedPad
                           then error $ "pkcs7Unpad: Malformed padding: " ++ show pad
                           else unpadded where
                               (unpadded, pad) = B.splitAt (len - padLen) bs
                               expectedPad     = B.replicate padLen lastByte
                               lastByte        = B.last bs
                               padLen          = fromIntegral lastByte
                               k'              = fromIntegral k
                               len             = B.length bs

-- | Decrypts a byte string with the given key using AES in ECB mode.
decryptAES_ECB      :: B.ByteString -> B.ByteString -> B.ByteString
decryptAES_ECB k bs = B.fromStrict $ decryptECB k' bs' where
    k' = initAES $ B.toStrict k
    bs' = B.toStrict bs

-- | Encrypts a byte string with the given keyV using AES in ECB mode.
encryptAES_ECB      :: B.ByteString -> B.ByteString -> B.ByteString
encryptAES_ECB k bs = B.fromStrict $ encryptECB k' bs' where
    k' = initAES $ B.toStrict k
    bs' = B.toStrict bs

-- | Decrypts a byte string with the given key and IV using AES in CBC mode.
decryptAES_CBC         :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
decryptAES_CBC k iv bs = B.concat $ zipWith go blocks (iv : blocks) where
    blocks      = chunks (fromIntegral $ B.length k) bs
    go          :: B.ByteString -> B.ByteString -> B.ByteString
    go blk prev = B.pack $ B.zipWith xor (decryptAES_ECB k blk) prev

-- | Encrypts a byte string with the given key and IV using AES in CBC mode.
encryptAES_CBC         :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
encryptAES_CBC k iv bs = (B.concat . drop 1 . reverse) (foldl go [iv] blocks) where
    blocks     = chunks (fromIntegral $ B.length k) bs
    go         :: [B.ByteString] -> B.ByteString -> [B.ByteString]
    go acc blk = encryptAES_ECB k (B.pack $ B.zipWith xor blk (head acc)) : acc

-- | Generates a random byte string of the given length.
randomBytes   :: Int -> IO B.ByteString
randomBytes n = do
  gen <- newStdGen
  return (B.pack $ take n (randoms gen))

-- | Generates a random AES-128 key.
randomAESKey :: IO B.ByteString
randomAESKey = randomBytes 16

-- | Generates a random AES-128 IV.
randomIV :: IO B.ByteString
randomIV = randomBytes 16

-- | Generates a random padding of variable length.
randomPadding     :: Int -> Int -> IO B.ByteString
randomPadding a b = do
  gen <- newStdGen
  randomBytes (fst $ randomR (a, b) gen)

-- | Generates a random bool.
randomBool :: IO Bool
randomBool = do
  gen <- newStdGen
  return (fst $ random gen)

-- | Map of all chunk frequencies of a given byte string.
--
-- Keys are the chunks, and values are their frequencies.
type ChunkFrequencies = Map.Map B.ByteString Integer

-- | Returns the chunk frequencies of a given length in a given byte
-- string.
chunkFrequencies      :: Integer -> B.ByteString -> ChunkFrequencies
chunkFrequencies n bs = foldl go Map.empty (chunks n bs) where
    go :: ChunkFrequencies -> B.ByteString -> ChunkFrequencies
    go freq chunk = case Map.lookup chunk freq of
                      Nothing -> Map.insert chunk 1 freq
                      Just n' -> Map.insert chunk (n' + 1) freq

-- | Detects whether a given byte string has been encrypted using ECB
-- mode with the given block size.
detectECB      :: Integer -> B.ByteString -> Maybe ChunkFrequencies
detectECB n bs = let freq = Map.filter (> 1) (chunkFrequencies n bs) in
                 if Map.null freq
                 then Nothing
                 else Just freq

-- | Oracle function for AES-128 encryption.
--
-- Pads the given plaintext with a random prefix and suffix of random
-- lengths between 5 and 10, randomly chooses an AES-128 encryption mode
-- (ECB or CBC) and a key, and encrypts the padded plaintext.
encryptionOracle    :: B.ByteString -> IO B.ByteString
encryptionOracle bs = do
  (ret, _) <- leakyEncryptionOracle bs
  return ret

data CipherMode = ECB | CBC
                deriving (Eq, Show)

-- | Version of @encryptionOracle@ that leaks the encryption mode used.
leakyEncryptionOracle    :: B.ByteString -> IO (B.ByteString, CipherMode)
leakyEncryptionOracle bs = do
  k <- randomAESKey
  p <- randomPadding 5 10
  s <- randomPadding 5 10
  iv <- randomIV
  cbc <- randomBool
  let encrypted =
           if cbc
           then encryptAES_CBC k iv bs
           else encryptAES_ECB k bs
      mode = if cbc then CBC else ECB
  return (B.concat [p, encrypted, s], mode)
