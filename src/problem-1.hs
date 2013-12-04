-- 1. Convert hex to base64 and back.
--
-- The string:
--
--     49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d
--
-- should produce:
--
--     SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t

import qualified Matasano as M

source :: String
source = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

dest :: String
dest = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

result :: String
result = case M.hexToBytes source of
           Left err -> "Nope: " ++ err
           Right bs -> let enc = M.bytesToBase64 bs
                           bs1 = M.base64ToBytes enc in
                       if enc /= dest
                       then "Nope: " ++ enc ++ " /= " ++ dest
                       else
                           case bs1 of
                             Left err  -> "Nope: " ++ err
                             Right bs2 -> let enc1 = M.bytesToHex bs2 in
                                          if enc1 == source
                                          then "OK"
                                          else "Nope: " ++ enc1 ++ " /= " ++ source

main :: IO ()
main =
  putStrLn result
