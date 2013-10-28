-- 11. Write an oracle function and use it to detect ECB.
--
-- Now that you have ECB and CBC working:
--
-- Write a function to generate a random AES key; that's just 16 random
-- bytes.
--
-- Write a function that encrypts data under an unknown key --- that is,
-- a function that generates a random key and encrypts under it.
--
-- The function should look like:
--
-- encryption_oracle(your-input)
--  => [MEANINGLESS JIBBER JABBER]
--
-- Under the hood, have the function APPEND 5-10 bytes (count chosen
-- randomly) BEFORE the plaintext and 5-10 bytes AFTER the plaintext.
--
-- Now, have the function choose to encrypt under ECB 1/2 the time, and
-- under CBC the other half (just use random IVs each time for CBC). Use
-- rand(2) to decide which to use.
--
-- Now detect the block cipher mode the function is using each time.

import qualified Data.ByteString.Lazy as B

import qualified Matasano as M

generatePlaintext :: IO B.ByteString
generatePlaintext = M.randomBytes (1024 * 16)

guessECB            :: (B.ByteString, M.CipherMode) -> Either String String
guessECB (bs, mode) = case M.detectECB 16 bs of
                        Nothing -> if mode == M.CBC
                                   then Right "OK"
                                   else Left "Detected ECB, expected CBC"
                        Just _  -> if mode == M.ECB
                                   then Right "OK"
                                   else Left "Detected CBC, expected ECB"

main :: IO ()
main = do
    bs <- generatePlaintext
    encrypted <- M.leakyEncryptionOracle bs
    print $ guessECB encrypted
