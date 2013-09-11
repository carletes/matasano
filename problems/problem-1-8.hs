-- 8. Detecting ECB
--
-- At the following URL are a bunch of hex-encoded ciphertexts:
--
--     https://gist.github.com/3132928
--
-- One of them is ECB encrypted. Detect it.
--
-- Remember that the problem with ECB is that it is stateless and
-- deterministic; the same 16 byte plaintext block will always produce
-- the same 16 byte ciphertext.

main :: IO ()
main = do
  putStrLn "Not yet"
