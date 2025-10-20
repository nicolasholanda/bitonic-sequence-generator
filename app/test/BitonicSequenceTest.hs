module Main where

import Test.HUnit
import BitonicSequence (bitonicArray)
import System.Exit (exitSuccess, exitFailure)

testBasicCase :: Test
testBasicCase = TestCase $ do
    let result = bitonicArray 5 3 10
    let expected = [9, 10, 9, 8, 7]
    assertEqual "bitonicArray 5 3 10 should return [9, 10, 9, 8, 7]" expected result

testSingleElement :: Test
testSingleElement = TestCase $ do
    let result = bitonicArray 1 5 10
    let expected = [10]  -- n=1 returns just [r]
    assertEqual "bitonicArray 1 5 10 should return [10]" expected result

testPossibleCase :: Test
testPossibleCase = TestCase $ do
    let result = bitonicArray 7 2 5
    let expected = [2, 3, 4, 5, 4, 3, 2]  -- This is possible: (5-2)*2+1 = 7
    assertEqual "bitonicArray 7 2 5 should return [2, 3, 4, 5, 4, 3, 2]" expected result

testImpossibleCase :: Test
testImpossibleCase = TestCase $ do
    let result = bitonicArray 20 1 5
    let expected = [-1]  -- Impossible: needs 20 but max is (5-1)*2+1 = 9
    assertEqual "bitonicArray 20 1 5 should return [-1] (impossible)" expected result

testSmallRange :: Test
testSmallRange = TestCase $ do
    let result = bitonicArray 3 1 3
    let expected = [2, 3, 2]
    assertEqual "bitonicArray 3 1 3 should return [2, 3, 2]" expected result

tests :: Test
tests = TestList
    [ TestLabel "Basic case" testBasicCase
    , TestLabel "Single element" testSingleElement
    , TestLabel "Possible case (7 2 5)" testPossibleCase
    , TestLabel "Impossible case (20 1 5)" testImpossibleCase
    , TestLabel "Small range" testSmallRange
    ]

-- Main function to run the test
main :: IO ()
main = do
    putStrLn "Running Bitonic Sequence Generator Tests"
    counts <- runTestTT tests
    putStrLn ""
    if errors counts + failures counts == 0
        then do
            putStrLn "[PASS] All tests passed!"
            exitSuccess
        else do
            putStrLn "[FAIL] Some tests failed!"
            exitFailure
