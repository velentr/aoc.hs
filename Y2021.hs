module Y2021
  ( d01a
  , d01b
  , d02a
  , d02b
  , d03a
  , d03b
  , d04a
  , d04b
  , d05a
  , d05b
  , d06a
  , d06b
  , d07a
  , d07b
  , d08a
  , d08b
  , d09a
  , d09b
  , d10a
  , d10b
  , d11a
  , d11b
  , d12a
  , d12b
  , d13a
  , d13b
  , d14a
  , d14b
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO as IO

import qualified Graph
import qualified Input
import qualified Queue

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

bingoNums :: String -> [Int]
bingoNums line =
  map read $ words [if c == ',' then ' ' else c | c <- line]

type Board = [Set.Set Int]

boardFromList :: [String] -> Board
boardFromList lst =
  let nums = map bingoNums lst
      rows = map Set.fromList nums
      cols = map Set.fromList [[row !! n | row <- nums] | n <- [0..4]]
  in rows ++ cols

boardsFromInput :: [String] -> [Board]
boardsFromInput ls
  | length ls < 5 = []
  | otherwise =
  let (b, remaining) = splitAt 5 ls
  in (boardFromList b):(boardsFromInput remaining)

boardIsWinner :: Set.Set Int -> Board -> Bool
boardIsWinner ns b =
  any (\s -> 5 == length (Set.intersection s ns)) b

boardScore :: Board -> [Int] -> Int
boardScore b called =
  let ns = Set.fromList called
  in (last called) * (sum $ Set.unions $ map (\w -> Set.difference w ns) b)

findBingoWinner :: [Board] -> [Int] -> Int -> Int
findBingoWinner bs called at =
  let ns = Set.fromList (take at called)
      winners = filter (boardIsWinner ns) bs
  in if length winners > 0 then
       boardScore (winners !! 0) (take at called)
     else
       findBingoWinner bs called (at + 1)

findLastBingoWinner :: [Board] -> [Int] -> Int -> Int
findLastBingoWinner bs called at =
  let ns = Set.fromList (take at called)
      losers = filter (\b -> not (boardIsWinner ns b)) bs
  in if length losers == 1 then
       findBingoWinner losers called (at + 1)
    else
       findLastBingoWinner losers called (at + 1)

d04a :: FilePath -> IO Int
d04a input = do
  (numsStr:_:boardStrs) <- Input.s input
  let nums = bingoNums numsStr
      boards = boardsFromInput [b | b <- boardStrs, b /= ""]
  return $ findBingoWinner boards nums 1

d04b :: FilePath -> IO Int
d04b input = do
  (numsStr:_:boardStrs) <- Input.s input
  let nums = bingoNums numsStr
      boards = boardsFromInput [b | b <- boardStrs, b /= ""]
  return $ findLastBingoWinner boards nums 1

data LineSegment = Vertical Int (Int, Int)
                 | Horizontal (Int, Int) Int
                 | Diagonal (Int, Int) (Int, Int) deriving Show
type LineMap = Map.Map (Int, Int) Int

parseLineSegment :: String -> LineSegment
parseLineSegment s
  | x0 == x1 = Vertical x0 (y0, y1)
  | y0 == y1 = Horizontal (x0, x1) y0
  | otherwise = Diagonal (x0, y0) (x1, y1)
  where [x0, y0, x1, y1] =
          map read $ words [if Char.isDigit c then c else ' ' | c <- s]

notDiagonal :: LineSegment -> Bool
notDiagonal (Diagonal _ _) = False
notDiagonal _ = True

range :: (Enum a, Ord a) => a -> a -> [a]
range n m
  | n > m = reverse [m..n]
  | otherwise = [n..m]

markPoint :: Int -> Int -> LineMap -> LineMap
markPoint x y m =
  Map.insert (x, y) (1 + Map.findWithDefault 0 (x, y) m) m

markLineSegment :: LineMap -> LineSegment -> LineMap
markLineSegment m0 (Vertical x (y0, y1)) =
  foldl (\m y -> markPoint x y m) m0 $ range y0 y1
markLineSegment m0 (Horizontal (x0, x1) y) =
  foldl (\m x -> markPoint x y m) m0 $ range x0 x1
markLineSegment m0 (Diagonal (x0, y0) (x1, y1)) =
  let points = zip (range x0 x1) (range y0 y1) in
  foldl (\m (x, y) -> markPoint x y m) m0 points

d05a :: FilePath -> IO Int
d05a input = do
  lineSegmentStrs <- Input.s input
  let lineSegments = map parseLineSegment lineSegmentStrs
      m = foldl markLineSegment Map.empty $ filter notDiagonal lineSegments
  return $ Map.size $ Map.filter (\v -> v >= 2) m

d05b :: FilePath -> IO Int
d05b input = do
  lineSegmentStrs <- Input.s input
  let lineSegments = map parseLineSegment lineSegmentStrs
      m = foldl markLineSegment Map.empty lineSegments
  return $ Map.size $ Map.filter (\v -> v >= 2) m

parseLanternFish :: FilePath -> IO [Int]
parseLanternFish input = do
  contents <- IO.readFile input
  let ages = [read n |
              n <- words [if Char.isDigit c then c else ' ' | c <- contents]]
      counts = foldl (\m n -> Map.insert n (1 + Map.findWithDefault 0 n m) m) Map.empty ages
  return [Map.findWithDefault 0 n counts | n <- [0..8]]

stepGeneration :: [Int] -> [Int]
stepGeneration (numOldies:others) =
  let gen6 = (others !! 6) + numOldies in
    (take 6 others) ++ [gen6, others !! 7, numOldies]

d06a :: FilePath -> IO Int
d06a input = do
  gen0 <- parseLanternFish input
  return $ sum $ foldl (\g _ -> stepGeneration g) gen0 [1..80]

d06b :: FilePath -> IO Int
d06b input = do
  gen0 <- parseLanternFish input
  return $ sum $ foldl (\g _ -> stepGeneration g) gen0 [1..256]


median :: [Int] -> Int
median ns
  | odd l = ns' !! (div l 2)
  | otherwise =
    let (smaller, larger) = splitAt (div l 2) ns'
    in div ((last smaller) + (head larger)) 2
  where ns' = List.sort ns
        l = length ns'

mean :: [Int] -> Int
mean ns =
  round $ (fromIntegral (sum ns)) / (fromIntegral (length ns))

csInt :: FilePath -> IO [Int]
csInt input = do
  contents <- IO.readFile input
  return [read n | n <- words [if Char.isDigit c then c else ' ' | c <- contents]]

d07a :: FilePath -> IO Int
d07a input = do
  crabPositions <- csInt input
  let m = median crabPositions
  return $ sum [abs (n - m) | n <- crabPositions]

cost :: Int -> Int
cost n = div (n * (n + 1)) 2

d07b :: FilePath -> IO Int
d07b input = do
  crabPositions <- csInt input
  let m = mean crabPositions
      mn = m - 1
      mp = m + 1
  return $ minimum [
    sum [cost (abs (n - m)) | n <- crabPositions],
    sum [cost (abs (n - mp)) | n <- crabPositions],
    sum [cost (abs (n - mn)) | n <- crabPositions]
    ]

parseSegmentLine :: String -> ([Set.Set Char], [Set.Set Char])
parseSegmentLine line =
  let (patterns, _:display) = break ((==) '|') line in
    (map Set.fromList (words patterns), map Set.fromList (words display))

countUniquePatterns :: [Set.Set Char] -> Int
countUniquePatterns display =
  length [0 | digit <- display,
          let len = length digit,
           or [len == 2, len == 3, len == 4, len == 7]]

d08a :: FilePath -> IO Int
d08a input = do
  notes <- Input.s input
  let displays = map parseSegmentLine notes
  return $ sum [countUniquePatterns display | (_, display) <- displays]

find1 :: [Set.Set Char] -> Set.Set Char
find1 patterns =
  head [s | s <- patterns, length s == 2]

find4 :: [Set.Set Char] -> Set.Set Char
find4 patterns =
  head [s | s <- patterns, length s == 4]

find7 :: [Set.Set Char] -> Set.Set Char
find7 patterns =
  head [s | s <- patterns, length s == 3]

find8 :: [Set.Set Char] -> Set.Set Char
find8 patterns =
  head [s | s <- patterns, length s == 7]

find0 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find0 patterns uniq =
  let Just p1 = Map.lookup 1 uniq
      Just p4 = Map.lookup 4 uniq
      bd = Set.difference p4 p1
  in head [s | s <- patterns, length s == 6, length (Set.intersection bd s) == 1]

find9 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find9 patterns uniq =
  let Just p4 = Map.lookup 4 uniq
  in head [s | s <- patterns, length s == 6, length (Set.intersection p4 s) == 4]

find6 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find6 patterns uniq =
  let Just p1 = Map.lookup 1 uniq
  in head [s | s <- patterns, length s == 6, length (Set.intersection p1 s) == 1]

find3 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find3 patterns uniq =
  let Just p1 = Map.lookup 1 uniq
  in head [s | s <- patterns, length s == 5, length (Set.intersection p1 s) == 2]

find2 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find2 patterns uniq =
  let Just p4 = Map.lookup 4 uniq
  in head [s | s <- patterns, length s == 5, length (Set.intersection p4 s) == 2]

find5 :: [Set.Set Char] -> Map.Map Int (Set.Set Char) -> Set.Set Char
find5 patterns uniq =
  let Just p1 = Map.lookup 1 uniq
      Just p4 = Map.lookup 4 uniq
  in head [s | s <- patterns, length s == 5,
           length (Set.intersection p4 s) == 3,
           length (Set.intersection p1 s) == 1]


decodeDisplay :: Map.Map (Set.Set Char) Int -> [Set.Set Char] -> Int
decodeDisplay decoder display =
  let digits = [d | digit <- display, let Just d = Map.lookup digit decoder] in
    sum [(10 ^ power) * digit | (power, digit) <- zip [0..] (reverse digits)]

decodeSegment :: [Set.Set Char] -> [Set.Set Char] -> Int
decodeSegment patterns display =
  let p1 = find1 patterns
      p4 = find4 patterns
      p7 = find7 patterns
      p8 = find8 patterns
      uniq = Map.fromList [(1, p1), (4, p4), (7, p7), (8, p8)]
      p0 = find0 patterns uniq
      p2 = find2 patterns uniq
      p3 = find3 patterns uniq
      p5 = find5 patterns uniq
      p6 = find6 patterns uniq
      p9 = find9 patterns uniq
      deduced = Map.fromList [(0, p0), (2, p2), (3, p3), (5, p5), (6, p6), (9, p9)]
      m = Map.union uniq deduced
      decoder = Map.fromList [(key, digit) | (digit, key) <- Map.toList m]
  in decodeDisplay decoder display

d08b :: FilePath -> IO Int
d08b input = do
  notes <- Input.s input
  let displays = map parseSegmentLine notes
  return $ sum [decodeSegment patterns display | (patterns, display) <- displays]

type HeightMap = Map.Map (Int, Int) Int

getHeight :: HeightMap -> Int -> Int -> Int
getHeight m x y =
  Map.findWithDefault 10 (x, y) m

isLowPoint :: HeightMap -> Int -> Int -> Bool
isLowPoint m x y =
  let h = getHeight m x y in
    and [h < getHeight m (x - 1) y,
         h < getHeight m x (y - 1),
         h < getHeight m (x + 1) y,
         h < getHeight m x (y + 1)]

getLowPoints :: HeightMap -> [(Int, Int)]
getLowPoints m =
  Map.keys $ Map.filterWithKey (\(x, y) _ -> isLowPoint m x y) m

type NumberGrid = Map.Map (Int, Int) Int

numberRow :: (Int, String) -> NumberGrid
numberRow (y, row) =
  foldl (\m (x, c) -> Map.insert (x, y) (Char.digitToInt c) m) Map.empty $ zip [0..] row

readNumberGrid :: FilePath -> IO NumberGrid
readNumberGrid input = do
  rows <- Input.s input
  return $ Map.unions [numberRow r | r <- zip [0..] rows]

riskLevel :: HeightMap -> Int -> Int -> Int
riskLevel m x y =
  1 + (getHeight m x y)

readHeightMap :: FilePath -> IO HeightMap
readHeightMap = readNumberGrid

d09a :: FilePath -> IO Int
d09a input = do
  hm <- readHeightMap input
  return $ sum [riskLevel hm x y | (x, y) <- getLowPoints hm]

type Basin = Set.Set (Int, Int)

growBasin :: HeightMap -> Basin -> [(Int, Int)] -> Basin
growBasin _ b [] = b
growBasin hm b ((x, y):rest)
  | ph > 8 || Set.member (x, y) b = growBasin hm b rest
  | otherwise =
    growBasin hm (Set.insert (x, y) b) ((x, y+1):(x+1, y):(x, y-1):(x-1, y):rest)
  where ph = getHeight hm x y

d09b :: FilePath -> IO Int
d09b input = do
  hm <- readHeightMap input
  let lps = getLowPoints hm
      basins = [growBasin hm Set.empty [lp] | lp <- lps]
      (b0:b1:b2:_) = reverse $ List.sort [Set.size basin | basin <- basins]
  return $ b0 * b1 * b2

data ParseResult = Ok String | Error Int | Incomplete String

isChunkOpener :: Char -> Bool
isChunkOpener '(' = True
isChunkOpener '[' = True
isChunkOpener '{' = True
isChunkOpener '<' = True
isChunkOpener _ = False

chunkMatches :: Char -> Char -> Bool
chunkMatches '(' ')' = True
chunkMatches '[' ']' = True
chunkMatches '{' '}' = True
chunkMatches '<' '>' = True
chunkMatches _ _ = False

chunkErrorScore :: Char -> Int
chunkErrorScore ')' = 3
chunkErrorScore ']' = 57
chunkErrorScore '}' = 1197
chunkErrorScore '>' = 25137
chunkErrorScore _ = error "invalid chunk closer"

chunkComplement :: Char -> Char
chunkComplement '(' = ')'
chunkComplement '[' = ']'
chunkComplement '{' = '}'
chunkComplement '<' = '>'
chunkComplement _ = error "invalid chunk opener"

parseChunk :: String -> ParseResult
parseChunk [] = Ok []
parseChunk (c:rest)
  | isChunkOpener c =
    case parseChunks rest of
      Error score -> Error score
      Ok [] -> Incomplete [chunkComplement c]
      Ok (c':remaining) ->
        if chunkMatches c c' then
          Ok remaining
        else
          Error (chunkErrorScore c')
      Incomplete required -> Incomplete ((chunkComplement c):required)
  | otherwise = Error (chunkErrorScore c)

parseChunks :: String -> ParseResult
parseChunks [] = Ok []
parseChunks (c:rest)
  | isChunkOpener c =
    case parseChunk (c:rest) of
      Ok remaining -> parseChunks remaining
      Error score -> Error score
      Incomplete required -> Incomplete required
  | otherwise = Ok (c:rest)

parseScore :: String -> Int
parseScore s =
  case parseChunk s of
    Ok [] -> 0
    Ok remaining -> parseScore remaining
    Incomplete _ -> 0
    Error score -> score

d10a :: FilePath -> IO Int
d10a input = do
  ls <- Input.s input
  return $ sum [parseScore l | l <- ls]

chunkIncompleteScore :: Char -> Int
chunkIncompleteScore ')' = 1
chunkIncompleteScore ']' = 2
chunkIncompleteScore '}' = 3
chunkIncompleteScore '>' = 4
chunkIncompleteScore _ = error "invalid chunk closer"

incompleteChunkScore :: String -> Int -> Int
incompleteChunkScore [] score = score
incompleteChunkScore (c:cs) score =
  incompleteChunkScore cs (5*score + chunkIncompleteScore c)

completionScore :: String -> Int
completionScore s =
  case parseChunk s of
    Ok remaining -> completionScore remaining
    Incomplete required ->
      incompleteChunkScore (reverse required) 0
    _ -> error "invalid completion?"

d10b :: FilePath -> IO Int
d10b input = do
  ls <- Input.s input
  let incompletes = [l | l <- ls, parseScore l == 0]
  return $ median [completionScore incomplete | incomplete <- incompletes]

activateNeighbors :: NumberGrid -> (Int, Int) -> NumberGrid
activateNeighbors m (x, y) =
  let neighbors = [(x+1, y)
                  ,(x+1, y+1)
                  ,(x,   y+1)
                  ,(x-1, y+1)
                  ,(x-1, y)
                  ,(x-1, y-1)
                  ,(x,   y-1)
                  ,(x+1, y-1)
                  ]
  in
    foldl (\m' (x', y') -> activateOctopus m' (x', y')) m neighbors

activateOctopus :: NumberGrid -> (Int, Int) -> NumberGrid
activateOctopus m (x, y) =
  case Map.lookup (x, y) m of
    Just 10 -> m
    Just 9 ->
      let m' = Map.insert (x, y) 10 m in
        activateNeighbors m' (x, y)
    Just n ->
      Map.insert (x, y) (n + 1) m
    Nothing -> m

stepOctopi :: (NumberGrid, Int) -> (NumberGrid, Int)
stepOctopi (m, n) =
  let m' = Map.map ((+) 1) m
      hiEnergy = Map.keys $ Map.filter ((==) 10) m'
      m'' = foldl activateNeighbors m' hiEnergy
      flashed = Map.size $ Map.filter ((==) 10) m''
  in
    (Map.map (\energy -> if energy == 10 then 0 else energy) m'', n + flashed)

d11a :: FilePath -> IO Int
d11a input = do
  m <- readNumberGrid input
  let (_, totalFlashes) = foldl (\m' _ -> stepOctopi m') (m, 0) [1..100]
  return totalFlashes

checkAllFlashed :: NumberGrid -> Int -> Int
checkAllFlashed m step =
  let (m', numFlashed) = stepOctopi (m, 0) in
    if numFlashed == Map.size m' then
      step
    else
      checkAllFlashed m' (step + 1)

d11b :: FilePath -> IO Int
d11b input = do
  m <- readNumberGrid input
  return $ checkAllFlashed m 1

parseGraph :: FilePath -> IO (Graph.Graph String)
parseGraph input = do
  ls <- Input.s input
  let edges = [(v0, v1) | l <- ls,
               let [v0, v1] = words [if Char.isAlpha c then c else ' ' | c <- l]]
      g = Graph.fromList edges
  return $ foldl Graph.addEdge g [(v1, v0) | (v0, v1) <- edges]

type Cave = String
type Path = [Cave]
type Paths = Set.Set Path

isSmallCave :: Cave -> Bool
isSmallCave "" = error "empty cave name?"
isSmallCave (c:_) = Char.isAsciiLower c

isInList :: (Eq a) => a -> [a] -> Bool
isInList elt l =
  List.any ((==) elt) l

canVisitA :: Cave -> Path -> Bool
canVisitA v path =
  (not $ isSmallCave v) || (not $ isInList v path)

hasDuplicateCave :: Path -> Bool
hasDuplicateCave p =
  let smallCaves = filter isSmallCave p
      uniq = Set.fromList smallCaves
  in Set.size uniq /= length smallCaves

canVisitB :: Cave -> Path -> Bool
canVisitB "start" [] = True
canVisitB "start" _ = False
canVisitB v path =
  (not $ isSmallCave v) || (not $ isInList v path) || (not $ hasDuplicateCave path)

findPathsStep :: Graph.Graph Cave -> (Cave -> Path -> Bool) -> Queue.Queue (Cave, Path) -> Paths -> Paths
findPathsStep g canVisit q soFar =
  let (it, q') = Queue.pop' q in
    case it of
      Nothing -> soFar
      Just ("end", path) ->
        findPathsStep g canVisit q' $ Set.insert path soFar
      Just (v, path) ->
        if not $ canVisit v path then
          findPathsStep g canVisit q' soFar
        else
          let neighbors = Graph.neighbors g v
              path' = v:path
              q'' = foldl Queue.push q' [(n, path') | n <- Set.toList neighbors]
          in findPathsStep g canVisit q'' soFar

findPaths :: Graph.Graph Cave -> (Cave -> Path -> Bool) -> Paths
findPaths g canVisit =
  findPathsStep g canVisit (Queue.fromList [("start", [])]) Set.empty

d12a :: FilePath -> IO Int
d12a input = do
  g <- parseGraph input
  return $ Set.size $ findPaths g canVisitA

-- SLOW
d12b :: FilePath -> IO Int
d12b input = do
  g <- parseGraph input
  return $ Set.size $ findPaths g canVisitB

type Dots = Set.Set (Int, Int)

splitOnBlank :: FilePath -> IO ([String], [String])
splitOnBlank input = do
  l <- Input.s input
  let (dots, ("":folds)) = break ((==) "") l
  return (dots, folds)

parseDots :: [String] -> [(Int, Int)]
parseDots dots =
  [(read x, read y) | xy <- dots, let (x, (',':y)) = break ((==) ',') xy]

data Fold = AlongX Int | AlongY Int deriving Show

parseFold :: String -> Fold
parseFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':y) = AlongY (read y)
parseFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':x) = AlongX (read x)
parseFold _ = error "invalid fold?"

parseFolds :: [String] -> [Fold]
parseFolds = map parseFold

parseDotFolds :: FilePath -> IO (Dots, [Fold])
parseDotFolds input = do
  (d, f) <- splitOnBlank input
  return (Set.fromList $ parseDots d, parseFolds f)

executeFold :: Dots -> Fold -> Dots
executeFold dots (AlongY y) =
  Set.map (\(x0, y0) -> (x0, if y0 > y then 2*y - y0 else y0)) dots
executeFold dots (AlongX x) =
  Set.map (\(x0, y0) -> (if x0 > x then 2*x - x0 else x0, y0)) dots

d13a :: FilePath -> IO Int
d13a input = do
  (dots, folds) <- parseDotFolds input
  return $ Set.size $ executeFold dots $ head folds

maxXY :: Dots -> (Int, Int)
maxXY dots =
  Set.foldl (\(maxX, maxY) (x, y) ->
               (if x > maxX then x else maxX,
                if y > maxY then y else maxY)) (0, 0) dots

putDot :: Dots -> Int -> Int -> IO ()
putDot dots x y =
  if Set.member (x, y) dots then
    putChar '#'
  else
    putChar '.'

putRow :: Dots -> Int -> Int -> IO ()
putRow dots y maxX = do
  sequence_ $ (map (\x -> putDot dots x y) [0..maxX]) ++ [(putStrLn "")]

printDots :: Dots -> Int -> Int -> IO ()
printDots dots maxX maxY = do
  mapM_ (\y -> putRow dots y maxX) [0..maxY]

-- how to test?
d13b :: FilePath -> IO ()
d13b input = do
  (dots, folds) <- parseDotFolds input
  let dots' = foldl executeFold dots folds
      (maxX, maxY) = maxXY dots'
  printDots dots' maxX maxY

type Insertions = Map.Map (Char, Char) Char
type Polymer = Map.Map (Char, Char) Int

parseInsertion :: Insertions -> String -> Insertions
parseInsertion m (i0:i1:' ':'-':'>':' ':o:[]) = Map.insert (i0, i1) o m
parseInsertion _ _ = error "invalid insertion sequence?"

parsePolymer :: String -> Polymer
parsePolymer p =
  countAll $ zip p $ tail p

insertPair :: Polymer -> Insertions -> (Char, Char) -> Int -> Polymer
insertPair pSoFar m (c0, c1) count =
  let Just c = Map.lookup (c0, c1) m
      p' = Map.insert (c0, c) (count + Map.findWithDefault 0 (c0, c) pSoFar) pSoFar
      p'' = Map.insert (c, c1) (count + Map.findWithDefault 0 (c, c1) p') p'
  in p''

insertPolymer :: Polymer -> Insertions -> Polymer
insertPolymer p m =
  Map.foldlWithKey (\p' pair count -> insertPair p' m pair count) Map.empty p

countAll :: (Ord a) => [a] -> Map.Map a Int
countAll elts =
  foldl (\m elt -> Map.insert elt (1 + Map.findWithDefault 0 elt m) m) Map.empty elts

countPair :: Map.Map Char Int -> (Char, Char) -> Int -> Map.Map Char Int
countPair m (c0, c1) count =
  let m' = Map.insert c0 (count + Map.findWithDefault 0 c0 m) m in
  Map.insert c1 (count + Map.findWithDefault 0 c1 m') m'

countElements :: Polymer -> Char -> Char -> Map.Map Char Int
countElements p c0 cl =
  let m = Map.foldlWithKey countPair Map.empty p in
    Map.mapWithKey (\c count ->
                      div
                      (count + (if c == c0 then 1 else 0) +
                       (if c == cl then 1 else 0)) 2) m

expandPolymer :: Polymer -> Insertions -> Int -> Char -> Char -> Int
expandPolymer template insertions iterations c0 cl =
  let p = foldl (\polymer _ -> insertPolymer polymer insertions) template [1..iterations]
      counts = countElements p c0 cl
  in (maximum $ Map.elems counts) - (minimum $ Map.elems counts)

d14a :: FilePath -> IO Int
d14a input = do
  ([templateStr], insertionStrs) <- splitOnBlank input
  let template = parsePolymer templateStr
      insertions = foldl parseInsertion Map.empty insertionStrs
  return $ expandPolymer template insertions 10 (head templateStr) (last templateStr)

d14b :: FilePath -> IO Int
d14b input = do
  ([templateStr], insertionStrs) <- splitOnBlank input
  let template = parsePolymer templateStr
      insertions = foldl parseInsertion Map.empty insertionStrs
  return $ expandPolymer template insertions 40 (head templateStr) (last templateStr)
