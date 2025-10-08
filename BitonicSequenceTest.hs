module Main where

import Test.HUnit
import BitonicSequence (bitonicArray)
import System.Exit (exitSuccess, exitFailure)

testBasicCase :: Test
testBasicCase = TestCase $ do
    let result = bitonicArray 5 3 10
    let expected = [9, 10, 9, 8, 7]
    assertEqual "bitonicArray 5 3 10 should return [9, 10, 9, 8, 7]" expected result

-- Main function to run the test
main :: IO ()
main = do
    putStrLn "Running Bitonic Sequence Generator Test"
    counts <- runTestTT testBasicCase
    putStrLn ""
    if errors counts + failures counts == 0
        then do
            putStrLn "✓ Test passed!"
            exitSuccess
        else do
            putStrLn "✗ Test failed!"
            exitFailure
