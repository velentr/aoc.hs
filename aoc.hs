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
                                 ]]

main :: IO ()
main = do
  testResults <- runTestTT allTests
  print testResults
