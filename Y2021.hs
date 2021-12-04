module Y2021
  ( d01a
  , d01b
  , d02a
  , d02b
  , d03a
  , d03b
  ) where

import qualified Data.Char as Char
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

data SubDirection = Forward Int | Up Int | Down Int deriving Read
type SubPosition = (Int, Int)
type SubAimedPosition = (Int, Int, Int)

capitalize :: String -> String
capitalize "" = ""
capitalize (c:rest) = (Char.toUpper c):rest

moveA :: SubPosition -> SubDirection -> SubPosition
moveA (h, d) dir =
  case dir of Forward x -> (h + x, d)
              Up x -> (h, d - x)
              Down x -> (h, d + x)

moveB :: SubAimedPosition -> SubDirection -> SubAimedPosition
moveB (h, d, a) dir =
  case dir of Forward x -> (h + x, d + (a * x), a)
              Up x -> (h, d, a - x)
              Down x -> (h, d, a + x)

travel :: FilePath -> (p -> SubDirection -> p) -> p -> IO p
travel input mover pos0 = do
  dirStrs <- Input.s input
  return $ foldl mover pos0 $ map (read . capitalize) dirStrs

d02a :: FilePath -> IO Int
d02a input = do
  (h, d) <- travel input moveA (0, 0)
  return $ h * d

d02b :: FilePath -> IO Int
d02b input = do
  (h, d, _) <- travel input moveB (0, 0, 0)
  return $ h * d

bin2int :: Num a => [a] -> a
bin2int ints =
  sum [b * r | (b, r) <- zip (reverse ints) [2 ^ n | n <- [0..]]]

mostCommonBit :: [[Int]] -> [Int]
mostCommonBit bs =
  let count1s = foldl1 (\a b -> [x + y | (x, y) <- zip a b]) bs
      len = length bs
  in [if n >= div (len + 1) 2 then 1 else 0 | n <- count1s]

readBits :: FilePath -> IO [[Int]]
readBits input = do
  str <- Input.s input
  return $ map (map Char.digitToInt) str

d03a :: FilePath -> IO Int
d03a input = do
  report <- readBits input
  let mcb = mostCommonBit report
      gamma = bin2int mcb
      epsilon = bin2int [1 - b | b <- mcb]
  return $ gamma * epsilon

filterMcb :: ([[Int]] -> [Int]) -> [[Int]] -> Int -> [Int]
filterMcb f nums i
  | (length nums) == 1 = nums !! 0
  | otherwise =
    let mcb = f nums
        next = filter (\bs -> (bs !! i) == (mcb !! i)) nums
    in filterMcb f next (i + 1)

leastCommonBit :: [[Int]] -> [Int]
leastCommonBit bs =
  [1 - b | b <- mostCommonBit bs]

d03b :: FilePath -> IO Int
d03b input = do
  report <- readBits input
  let ogr = filterMcb mostCommonBit report 0
      csr = filterMcb leastCommonBit report 0
  return $ (bin2int ogr) * (bin2int csr)
