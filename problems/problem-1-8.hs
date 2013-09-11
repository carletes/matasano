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

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import qualified Matasano as M
import Matasano.Utils (usage)

getStrings :: String -> Either String [B.ByteString]
getStrings body = sequence $ take 1000 $ map M.hexToBytes (lines body)

type ChunkFrequencies = Map.Map B.ByteString Integer

chunkFreq      :: Integer -> B.ByteString -> ChunkFrequencies
chunkFreq n bs = foldl go Map.empty (M.chunks n bs) where
    go :: ChunkFrequencies -> B.ByteString -> ChunkFrequencies
    go freq chunk = case Map.lookup chunk freq of
                      Nothing -> Map.insert chunk 1 freq
                      Just n  -> Map.insert chunk (n + 1) freq

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [dataFile] -> do
              body <- readFile dataFile
              let input = getStrings body
              case input of
                Left err -> do
                           putStrLn $ "Malformed input: " ++ err
                           exitWith $ ExitFailure 1
                Right input' -> do
                           mapM_ (putStrLn . show . chunkFreq 4) input'

    _ -> usage "<data file>"
