--- 4. Detect single-character XOR
---
--- One of the 60-character strings at:
---
---   https://gist.github.com/3132713
---
--- has been encrypted by single-character XOR. Find it. (Your code from
--- #3 should help.)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

getStrings :: IO [B.ByteString]
getStrings = do
  body <- B.readFile "data/gist-3132713"
  return (C.split '\n' body)

main = do
  result <- getStrings
  putStrLn $ show result
