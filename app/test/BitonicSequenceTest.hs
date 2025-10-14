module Main where

import Test.HUnit
import BitonicSequence (bitonicArray)
import System.Exit (exitSuccess, exitFailure)

testBasicCase :: Test
testBasicCase = TestCase $ do
    let result = bitonicArray 5 3 10
    let expected = [9, 10, 9, 8, 7]
    assertEqual "bitonicArray 5 3 10 should return [9, 10, 9, 8, 7]" expected result

testOtherBasicCase :: Test
testOtherBasicCase = TestCase $ do
    let result = bitonicArray 5 3 10
    let expected = [9, 10, 9, 8, 7]
    assertEqual "bitonicArray 5 3 10 should return [9, 10, 9, 8, 7]" expected result

testImpossibleCase :: Test
testImpossibleCase = TestCase $ do
    let result = bitonicArray 7 2 5
    assertEqual "bitonicArray 7 2 5 should return [2, 3, 4, 5, 4, 3, 2] (impossible)" [2, 3, 4, 5, 4, 3, 2] result

tests :: Test
tests = TestList
    [ TestLabel "Basic case" testBasicCase
    , TestLabel "Impossible case" testImpossibleCase
    , TestLabel "Other basic case" testOtherBasicCase
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
