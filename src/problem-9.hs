-- 9. Implement PKCS#7 padding
--
-- Pad any block to a specific block length, by appending the number of
-- bytes of padding to the end of the block. For instance,
--
--   "YELLOW SUBMARINE"
--
-- padded to 20 bytes would be:
--
--   "YELLOW SUBMARINE\x04\x04\x04\x04"
--
-- The particulars of this algorithm are easy to find online.

import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Matasano as M

main :: IO ()
main = do
  let input = C.pack "YELLOW SUBMARINE"
      padded = M.pkcs7Pad 20 input
      expected = B.append input (B.replicate 4 0x04)
  case padded of
    Left err ->
             do
               putStrLn $ "Error: " ++ show err
               exitWith $ ExitFailure 1
    Right padded ->
             if padded /= expected
             then do
               putStrLn $ "Error: " ++ show padded ++ " /= " ++ show expected
               exitWith $ ExitFailure 1
             else
                 putStrLn "OK"
