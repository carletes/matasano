--- 5. Repeating-key XOR Cipher
---
--- Write the code to encrypt the string:
---
---   Burning 'em, if you ain't quick and nimble
---   I go crazy when I hear a cymbal
---
--- Under the key "ICE", using repeating-key XOR. It should come out to:
---
---   0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527
---   2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f
---
--- Encrypt a bunch of stuff using your repeating-key XOR function. Get a
--- feel for it.

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Matasano as M

input :: String
input = "Burning 'em, if you ain't quick and nimble\n" ++
        "I go crazy when I hear a cymbal"

key :: String
key = "ICE"

expected :: String
expected = "0b3637272a2b2e63622c2e69692a23693a2a3" ++
           "c6324202d623d63343c2a2622632427276527" ++
           "2a282b2f20430a652e2c652a3124333a653e2" ++
           "b2027630c692b20283165286326302e27282f"

result :: String
result = M.bytesToHex $ M.xorEncrypt (C.pack input) key' where
    key' :: B.ByteString
    key' = (C.pack . concat . repeat) key

main :: IO ()
main =
    if expected == result
    then do
      putStrLn "OK"
    else do
      putStrLn $ "Error: " ++ result ++ " /= " ++ expected
