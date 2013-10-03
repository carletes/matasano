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

import qualified Matasano as M
import Matasano.Utils (usage)

getStrings :: String -> Either String [B.ByteString]
getStrings body = mapM M.hexToBytes (lines body)

candidates      :: Integer -> B.ByteString -> Maybe (String, M.ChunkFrequencies)
candidates n bs = case M.detectECB n bs of
                    Nothing    -> Nothing
                    Just freqs -> Just (M.bytesToHex bs, freqs)

printCandidate                   :: Maybe (String, M.ChunkFrequencies) -> IO ()
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
                           let cs = filter notEmpty $ map (candidates 4) input'
                               notEmpty Nothing = False
                               notEmpty _       = True
                           case cs of
                             [] -> do
                                  putStrLn "No candidates found"
                                  exitWith $ ExitFailure 1
                             cs' ->
                                 mapM_ printCandidate cs'

    _ -> usage "<data file>"
