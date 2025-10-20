module Main where

import Test.HUnit
import qualified Database.Redis as Redis
import qualified Data.ByteString.Char8 as BS
import BitonicModels (BitonicRequest(..))
import BitonicRepository
import AppContext (AppEnv(..), runApp)
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (try, SomeException)

import qualified BitonicRepository as Repo

connectRedis :: IO AppEnv
connectRedis = do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    _ <- Redis.runRedis conn $ Redis.flushdb
    pure $ AppEnv conn

mkReq :: Int -> Int -> Int -> BitonicRequest
mkReq n l r = BitonicRequest { n = n, l = l, r = r }

testMakeRedisKey :: Test
testMakeRedisKey = TestCase $ do
    let k = makeRedisKey (mkReq 5 3 10)
    assertEqual "Key format" (BS.pack "bitonic:5:3:10") k

testFindMiss :: Test
testFindMiss = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping find miss test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            m <- runApp env $ Repo.findBitonic (mkReq 1 0 0)
            assertEqual "Missing key should return Nothing" Nothing m

testSaveAndFind :: Test
testSaveAndFind = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping save/find test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let req = mkReq 3 1 4
            let seqVal = [4,3,2]
            runApp env $ Repo.saveBitonic req seqVal
            m <- runApp env $ Repo.findBitonic req
            assertEqual "Saved value should be retrievable" (Just seqVal) m

testOverwrite :: Test
testOverwrite = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping overwrite test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let req = mkReq 2 0 1
            runApp env $ Repo.saveBitonic req [1,0]
            runApp env $ Repo.saveBitonic req [2,1]
            m <- runApp env $ Repo.findBitonic req
            assertEqual "Overwrite should reflect latest value" (Just [2,1]) m

testIndependence :: Test
testIndependence = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping independence test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let reqA = mkReq 2 5 9
            let reqB = mkReq 2 0 1
            runApp env $ Repo.saveBitonic reqA [9,8]
            runApp env $ Repo.saveBitonic reqB [1,0]
            a <- runApp env $ Repo.findBitonic reqA
            b <- runApp env $ Repo.findBitonic reqB
            assertEqual "A value" (Just [9,8]) a
            assertEqual "B value" (Just [1,0]) b
            assertBool "Different cached values" (a /= b)

testEmptyList :: Test
testEmptyList = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping empty list test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let req = mkReq 0 0 0
            runApp env $ Repo.saveBitonic req []
            m <- runApp env $ Repo.findBitonic req
            assertEqual "Empty list should round-trip" (Just []) m

tests :: Test
tests = TestList
    [ TestLabel "makeRedisKey" testMakeRedisKey
    , TestLabel "find miss" testFindMiss
    , TestLabel "save and find" testSaveAndFind
    , TestLabel "overwrite" testOverwrite
    , TestLabel "independence" testIndependence
    , TestLabel "empty list" testEmptyList
    ]

main :: IO ()
main = do
    putStrLn "Running Bitonic Repository Tests"
    counts <- runTestTT tests
    putStrLn ""
    if errors counts + failures counts == 0
        then do
            putStrLn "[PASS] All BitonicRepository tests passed!"
            exitSuccess
        else do
            putStrLn "[FAIL] Some BitonicRepository tests failed!"
            exitFailure
