-- 7. AES in ECB Mode
--
-- The Base64-encoded content at the following location:
--
--     https://gist.github.com/3132853
--
-- Has been encrypted via AES-128 in ECB mode under the key
--
--    "YELLOW SUBMARINE".
--
-- (I like "YELLOW SUBMARINE" because it's exactly 16 bytes long).
--
-- Decrypt it.
--
-- Easiest way:
--
-- Use OpenSSL::Cipher and give it AES-128-ECB as the cipher.

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Matasano as M
import Matasano.Utils

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
                        dec = M.decryptAES_ECB key input'
                        enc = M.encryptAES_ECB key dec
                    if enc /= input'
                       then do
                         putStrLn $ "Error: encrypt(decrypt(input)) /= input"
                         exitWith $ ExitFailure 1
                       else
                         putStrLn $ C.unpack dec
    _ -> usage "<data file>"
