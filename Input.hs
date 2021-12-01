module Input
  ( l
  ) where

import qualified System.IO as IO

l :: Read a => FilePath -> IO [a]
l filename = do
  contents <- IO.readFile filename
  return $ map read $ lines contents
