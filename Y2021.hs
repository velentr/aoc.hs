module Y2021
  ( d01a
  , d01b
  ) where

import qualified Input

countIncreases :: (Ord a, Num b) => [a] -> b
countIncreases nums =
  sum [if n < m then 1 else 0 | (n, m) <- zip nums $ tail nums]

d01a :: FilePath -> IO Int
d01a input = do
  report <- Input.l input :: IO [Int]
  return $ countIncreases report

d01b :: FilePath -> IO Int
d01b input = do
  report <- Input.l input :: IO [Int]
  let second = (tail report) ++ [0]
      third = (tail (tail report)) ++ [0, 0]
      windowSum = [x + y + z | (x, y, z) <- zip3 report second third]
  return $ countIncreases windowSum
