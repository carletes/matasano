-- Test suite for ``Matasano``.

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import qualified Data.ByteString.Lazy as B

import qualified Matasano as M

main :: IO()
main = defaultMain tests

tests = [
        testGroup "PKCS#1" [
                testProperty "Valid padding length" prop_pkcs7_valid_pad
            ]
        ]

-- | The PKCS#7 padding multiple must be in the range @[1, 256]@
prop_pkcs7_valid_pad k bs =
    ((k < 1) || (k > 256)) ==>
    M.pkcs7Pad k bs == Left "boo"
        where types = (k::Integer, bs::B.ByteString)

instance Arbitrary B.ByteString where
    arbitrary   = fmap B.pack arbitrary