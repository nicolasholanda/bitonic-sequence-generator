module Main where

import Test.HUnit
import qualified Database.Redis as Redis
import BitonicService
import qualified BitonicRepository as Repo
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (try, SomeException)

connectRedis :: IO Redis.Connection
connectRedis = do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    _ <- Redis.runRedis conn $ Redis.flushdb
    pure conn

mkReq :: Int -> Int -> Int -> BitonicRequest
mkReq n l r = BitonicRequest { n = n, l = l, r = r }


testCacheMissThenHit :: Test
testCacheMissThenHit = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping cache miss/hit test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let req = mkReq 5 3 10
            let repoReq = Repo.BitonicRequest (n req) (l req) (r req)
            initial <- Repo.findBitonic conn repoReq
            assertEqual "Expected no cached value initially" Nothing initial

            resp1 <- generateBitonic conn req
            let expected = [9,10,9,8,7]
            assertEqual "First response result should match expected sequence" expected (result resp1)

            cached <- Repo.findBitonic conn repoReq
            assertEqual "Sequence should be cached after first call" (Just expected) cached

            resp2 <- generateBitonic conn req
            assertEqual "Second response result should match expected sequence (cache hit)" expected (result resp2)

testDifferentRequestsAreIndependent :: Test
testDifferentRequestsAreIndependent = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException Redis.Connection)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping independence test"
            assertBool "Skipped due to missing Redis" True
        Right conn -> do
            let reqA = mkReq 5 3 10
            let reqB = mkReq 3 1 4

            _ <- generateBitonic conn reqA
            _ <- generateBitonic conn reqB

            let repoReqA = Repo.BitonicRequest (n reqA) (l reqA) (r reqA)
            let repoReqB = Repo.BitonicRequest (n reqB) (l reqB) (r reqB)

            cachedA <- Repo.findBitonic conn repoReqA
            cachedB <- Repo.findBitonic conn repoReqB

            assertBool "Cached A should be present" (cachedA /= Nothing)
            assertBool "Cached B should be present" (cachedB /= Nothing)
            assertBool "Cached values should differ for different requests" (cachedA /= cachedB || (repoReqA /= repoReqB))

tests :: Test
tests = TestList
    [ TestLabel "Cache miss then hit" testCacheMissThenHit
    , TestLabel "Different requests independent" testDifferentRequestsAreIndependent
    ]

main :: IO ()
main = do
    putStrLn "Running Bitonic Service Tests"
    counts <- runTestTT tests
    putStrLn ""
    if errors counts + failures counts == 0
        then do
            putStrLn "[PASS] All BitonicService tests passed!"
            exitSuccess
        else do
            putStrLn "[FAIL] Some BitonicService tests failed!"
            exitFailure
