module Main where

import Test.HUnit
import qualified Database.Redis as Redis
import qualified Data.ByteString.Char8 as BS
import BitonicRepository
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (try, SomeException)

import qualified BitonicRepository as Repo

connectRedis :: IO Redis.Connection
connectRedis = do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    _ <- Redis.runRedis conn $ Redis.flushdb
    pure conn

mkReq :: Int -> Int -> Int -> Repo.BitonicRequest
mkReq n l r = Repo.BitonicRequest { Repo.n = n, Repo.l = l, Repo.r = r }

testMakeRedisKey :: Test
testMakeRedisKey = TestCase $ do
    let k = makeRedisKey (mkReq 5 3 10)
    assertEqual "Key format" (BS.pack "bitonic:5:3:10") k

testFindMiss :: Test
testFindMiss = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping find miss test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            m <- Repo.findBitonic conn (mkReq 1 0 0)
            assertEqual "Missing key should return Nothing" Nothing m

testSaveAndFind :: Test
testSaveAndFind = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping save/find test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let req = mkReq 3 1 4
            let seqVal = [4,3,2]
            Repo.saveBitonic conn req seqVal
            m <- Repo.findBitonic conn req
            assertEqual "Saved value should be retrievable" (Just seqVal) m

testOverwrite :: Test
testOverwrite = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping overwrite test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let req = mkReq 2 0 1
            Repo.saveBitonic conn req [1,0]
            Repo.saveBitonic conn req [2,1]
            m <- Repo.findBitonic conn req
            assertEqual "Overwrite should reflect latest value" (Just [2,1]) m

testIndependence :: Test
testIndependence = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping independence test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let reqA = mkReq 2 5 9
            let reqB = mkReq 2 0 1
            Repo.saveBitonic conn reqA [9,8]
            Repo.saveBitonic conn reqB [1,0]
            a <- Repo.findBitonic conn reqA
            b <- Repo.findBitonic conn reqB
            assertEqual "A value" (Just [9,8]) a
            assertEqual "B value" (Just [1,0]) b
            assertBool "Different cached values" (a /= b)

testEmptyList :: Test
testEmptyList = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping empty list test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let req = mkReq 0 0 0
            Repo.saveBitonic conn req []
            m <- Repo.findBitonic conn req
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
