import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO as IO
import Test.HUnit

import qualified Input

import qualified Y2021

testDay :: (Eq b, Show b, Read b) => (FilePath -> IO b) -> FilePath -> IO ()
testDay f day = do
  let vectors = [day ++ "-actual.txt", day ++ "-sample.txt"]
  actual <- mapM f ["input/" ++ vector | vector <- vectors]
  expected <- mapM IO.readFile ["output/" ++ vector | vector <- vectors]
  assertEqual day actual $ map read expected

allTests :: Test
allTests = TestList [TestLabel day $ TestCase (testDay f day) |
                     (f, day) <- [ (Y2021.d01a, "2021-01a")
                                 , (Y2021.d01b, "2021-01b")
                                 , (Y2021.d02a, "2021-02a")
                                 , (Y2021.d02b, "2021-02b")
                                 , (Y2021.d03a, "2021-03a")
                                 , (Y2021.d03b, "2021-03b")
                                 , (Y2021.d04a, "2021-04a")
                                 , (Y2021.d04b, "2021-04b")
                                 , (Y2021.d05a, "2021-05a")
                                 , (Y2021.d05b, "2021-05b")
                                 , (Y2021.d06a, "2021-06a")
                                 , (Y2021.d06b, "2021-06b")
                                 , (Y2021.d07a, "2021-07a")
                                 , (Y2021.d07b, "2021-07b")
                                 , (Y2021.d08a, "2021-08a")
                                 , (Y2021.d08b, "2021-08b")
                                 ]]

main :: IO ()
main = do
  testResults <- runTestTT allTests
  print testResults
