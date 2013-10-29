-- Test suite for ``Matasano``.

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import qualified Data.ByteString.Lazy as B

import qualified Matasano as M

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

main :: IO()
main = defaultMain tests

tests = [
        testGroup "PKCS#7" [
                testProperty "Valid padding length" prop_pkcs7_valid_pad,
                testProperty "Length of padded string" prop_pkcs7_len
            ]
        ]

pkcs7_valid_pad k = (k >= 1) && (k <= 256)

-- The PKCS#7 padding multiple must be in the range @[1, 256]@
prop_pkcs7_valid_pad k bs =
    (not $ pkcs7_valid_pad k) ==>
    M.pkcs7Pad k bs == Left "boo" where
        types = (k::Integer, bs::B.ByteString)

-- The length of a padded byte string  is a multiple of the padding length.
prop_pkcs7_len k bs =
    (pkcs7_valid_pad k) ==>
    padLen == len + k' - (len `mod` k') where
        padLen = case M.pkcs7Pad k bs of
                   Right padded -> B.length padded
                   Left err     -> error err
        len    = B.length bs
        k'     = fromIntegral k
        types  = (k::Integer, bs::B.ByteString)

-- Unpadding a padded string returns the original string.
prop_pkcs7_identity k bs =
    (pkcs7_valid_pad k) ==>
    M.pkcs7Unpad k (padded) == Right bs where
        padded = case M.pkcs7Pad k bs of
                   Right bs' -> bs'
                   Left err  -> error err
        types = (k::Integer, bs::B.ByteString)