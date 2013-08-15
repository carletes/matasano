-- 2. Fixed XOR
--
-- Write a function that takes two equal-length buffers and produces
-- their XOR sum.
--
-- The string:
--
--     1c0111001f010100061a024b53535009181c
--
-- after hex decoding, when xor'd against:
--
--     686974207468652062756c6c277320657965
--
-- should produce:
--
--     746865206b696420646f6e277420706c6179

import qualified Matasano as M

input = M.hexToBytes "1c0111001f010100061a024b53535009181c"
key = M.hexToBytes "686974207468652062756c6c277320657965"
expected = "746865206b696420646f6e277420706c6179"

result = case input of
           Left err -> "Bad input: " ++ err
           Right input' -> case key of
                             Left err -> "Bad key: " ++ err
                             Right key' -> let encrypted = M.bytesToHex $ M.xorEncrypt input' key' in
                                           if encrypted == expected
                                           then "OK"
                                                else "Error: " ++ encrypted ++ " /= " ++ expected

main = do
  putStrLn result
