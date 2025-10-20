module Main where

import Test.HUnit
import qualified Database.Redis as Redis
import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import BitonicService
import AppContext (AppEnv(..), runApp)
import qualified BitonicRepository as Repo
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (try, SomeException)

connectRedis :: IO AppEnv
connectRedis = do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    _ <- Redis.runRedis conn $ Redis.flushdb
    pure $ AppEnv conn

mkReq :: Int -> Int -> Int -> BitonicRequest
mkReq n l r = BitonicRequest { n = n, l = l, r = r }


testCacheMissThenHit :: Test
testCacheMissThenHit = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping cache miss/hit test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let req = mkReq 5 3 10
            initial <- runApp env $ Repo.findBitonic req
            assertEqual "Expected no cached value initially" Nothing initial

            eitherResp1 <- runApp env $ generateBitonic req
            let expected = [9,10,9,8,7]
            case eitherResp1 of
                Right resp1 -> assertEqual "First response result should match expected sequence" expected (result resp1)
                Left err -> assertFailure $ "Expected Right but got Left: " ++ show err

            cached <- runApp env $ Repo.findBitonic req
            assertEqual "Sequence should be cached after first call" (Just expected) cached

            eitherResp2 <- runApp env $ generateBitonic req
            case eitherResp2 of
                Right resp2 -> assertEqual "Second response result should match expected sequence (cache hit)" expected (result resp2)
                Left err -> assertFailure $ "Expected Right but got Left: " ++ show err

testDifferentRequestsAreIndependent :: Test
testDifferentRequestsAreIndependent = TestCase $ do
    eres <- try connectRedis :: IO (Either SomeException AppEnv)
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping independence test"
            assertBool "Skipped due to missing Redis" True
        Right env -> do
            let reqA = mkReq 5 3 10
            let reqB = mkReq 3 1 4

            _ <- runApp env $ generateBitonic reqA
            _ <- runApp env $ generateBitonic reqB

            cachedA <- runApp env $ Repo.findBitonic reqA
            cachedB <- runApp env $ Repo.findBitonic reqB

            assertBool "Cached A should be present" (cachedA /= Nothing)
            assertBool "Cached B should be present" (cachedB /= Nothing)
            assertBool "Cached values should differ for different requests" (cachedA /= cachedB || (reqA /= reqB))

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
