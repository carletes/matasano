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
getStrings body = mapM M.hexToBytes (lines body)

type ChunkFrequencies = Map.Map B.ByteString Integer

chunkFreq      :: Integer -> B.ByteString -> ChunkFrequencies
chunkFreq n bs = foldl go Map.empty (M.chunks n bs) where
    go :: ChunkFrequencies -> B.ByteString -> ChunkFrequencies
    go freq chunk = case Map.lookup chunk freq of
                      Nothing -> Map.insert chunk 1 freq
                      Just n' -> Map.insert chunk (n' + 1) freq

candidate      :: Integer -> B.ByteString -> Maybe (String, ChunkFrequencies)
candidate n bs = let freq = Map.filter (> 1) (chunkFreq n bs) in
                 if Map.null freq
                 then Nothing
                 else Just (M.bytesToHex bs, freq)

printCandidate                   :: Maybe (String, ChunkFrequencies) -> IO ()
printCandidate Nothing           = putStrLn "(Nothing)"
printCandidate (Just (bs, freq)) =  do
  putStrLn $ "Candidate: " ++ bs
  putStrLn $ "Blocks:    " ++ show freq

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
                           let candidates = filter notEmpty $ map (candidate 4) input'
                               notEmpty Nothing = False
                               notEmpty _       = True
                           case candidates of
                             [] -> do
                                  putStrLn "No candidates found"
                                  exitWith $ ExitFailure 1
                             cs ->
                                 mapM_ printCandidate cs

    _ -> usage "<data file>"
