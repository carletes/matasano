-- 10. Implement CBC Mode
--
-- In CBC mode, each ciphertext block is added to the next plaintext
-- block before the next call to the cipher core.
--
-- The first plaintext block, which has no associated previous ciphertext
-- block, is added to a "fake 0th ciphertext block" called the IV.
--
-- Implement CBC mode by hand by taking the ECB function you just wrote,
-- making it encrypt instead of decrypt (verify this by decrypting
-- whatever you encrypt to test), and using your XOR function from
-- previous exercise.
--
-- DO NOT CHEAT AND USE OPENSSL TO DO CBC MODE, EVEN TO VERIFY YOUR
-- RESULTS. What's the point of even doing this stuff if you aren't going
-- to learn from it?
--
-- The buffer at:
--
--     https://gist.github.com/3132976
--
-- is intelligible (somewhat) when CBC decrypted against "YELLOW
-- SUBMARINE" with an IV of all ASCII 0 (\x00\x00\x00 &c)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Matasano as M
import Matasano.Utils (usage)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [dataFile] -> do
              input <- readFile dataFile
              case ((M.base64ToBytes . concat . lines) input) of
                Left err -> do
                    putStrLn $ "Malformed input: " ++ err
                    exitWith $ ExitFailure 1
                Right input' -> do
                    let key = C.pack "YELLOW SUBMARINE"
                        iv = B.replicate 16 0
                        dec = M.decryptAES_CBC key iv input'
                        enc = M.encryptAES_CBC key iv dec
                    if enc /= input'
                       then do
                         putStrLn $ "Error: encrypt(decrypt(input)) /= input"
                         putStrLn $ "encrypt(decrypt(input)): " ++ (show enc)
                         exitWith $ ExitFailure 1
                       else
                         putStrLn $ M.bytesToASCII dec
    _ -> usage "<data file>"