module Input
  ( l
  , s
  ) where

import qualified System.IO as IO

s :: FilePath -> IO [String]
s filename = do
  contents <- IO.readFile filename
  return $ lines contents

l :: Read a => FilePath -> IO [a]
l filename = do
  contents <- s filename
  return $ map read contents
