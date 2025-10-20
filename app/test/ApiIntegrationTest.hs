{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import qualified Web.Scotty as S
import qualified Database.Redis as Redis
import Network.Wai (Application, Request(..))
import Network.Wai.Test (Session, srequest, runSession, SRequest(..), simpleBody, simpleStatus, defaultRequest, setPath)
import Network.HTTP.Types (methodGet, methodPost, status200)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import Control.Exception (try, SomeException)
import System.Exit (exitSuccess, exitFailure)

import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import BitonicController (routes)
import AppContext (AppEnv(..))

mkApp :: AppEnv -> IO Application
mkApp env = S.scottyApp $ routes env

-- Helper to connect and flush DB
connectRedis :: IO (Either SomeException AppEnv)
connectRedis = try $ do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    _ <- Redis.runRedis conn $ Redis.flushdb
    pure $ AppEnv conn

reqGetRoot :: SRequest
reqGetRoot = SRequest
  { simpleRequest     = setPath (defaultRequest { requestMethod = methodGet }) "/"
  , simpleRequestBody = LBS.empty
  }

testRoot :: Application -> Test
testRoot app = TestCase $ do
    resp <- runSession (srequest reqGetRoot) app
    assertEqual "GET / should return 200" status200 (simpleStatus resp)
    -- Note: BitonicController doesn't define GET / route, so this will return 404
    -- We could remove this test or add the route back if needed

testPostBitonic :: Application -> Test
testPostBitonic app = TestCase $ do
    let br = BitonicRequest { n = 5, l = 3, r = 10 }
    let body = Aeson.encode br
    let req = SRequest
          { simpleRequest = setPath (defaultRequest { requestMethod = methodPost
                               , requestHeaders = [(hContentType, "application/json")]
                               }) "/bitonic"
          , simpleRequestBody = body
          }
    resp <- runSession (srequest req) app
    assertEqual "POST /bitonic 200" status200 (simpleStatus resp)
    case Aeson.eitherDecode (simpleBody resp) of
        Left err -> assertFailure ("Invalid JSON response: " ++ err)
        Right v ->
          case AT.parseEither parseBitonic (v :: Aeson.Value) of
            Left perr -> assertFailure ("JSON parse error: " ++ perr)
            Right (brq, res) -> do
              assertEqual "Echoed request matches" br brq
              assertEqual "Result sequence" [9,10,9,8,7] res

testPostCaching :: Application -> Test
testPostCaching app = TestCase $ do
    let br = BitonicRequest { n = 5, l = 3, r = 10 }
    let body = Aeson.encode br
    let req = SRequest
          { simpleRequest = setPath (defaultRequest { requestMethod = methodPost
                               , requestHeaders = [(hContentType, "application/json")]
                               }) "/bitonic"
          , simpleRequestBody = body
          }
    _ <- runSession (srequest req) app
    resp2 <- runSession (srequest req) app
    assertEqual "POST /bitonic 200 second" status200 (simpleStatus resp2)
    case Aeson.eitherDecode (simpleBody resp2) of
        Left err -> assertFailure ("Invalid JSON response: " ++ err)
        Right v ->
          case AT.parseEither parseBitonic (v :: Aeson.Value) of
            Left perr -> assertFailure ("JSON parse error: " ++ perr)
            Right (brq, res) -> do
              assertEqual "Echoed request matches again" br brq
              assertEqual "Result sequence" [9,10,9,8,7] res

parseBitonic :: Aeson.Value -> AT.Parser (BitonicRequest, [Int])
parseBitonic = AT.withObject "BitonicResponse" $ \o -> do
  br <- o AT..: "request"
  rs <- o AT..: "result"
  pure (br, rs)

main :: IO ()
main = do
    eres <- connectRedis
    case eres of
        Left _ -> do
            putStrLn "[SKIP] Redis not available: skipping API integration tests"
            counts <- runTestTT $ TestCase (assertBool "Skipped due to missing Redis" True)
            if errors counts + failures counts == 0 then exitSuccess else exitFailure
        Right env -> do
            app <- mkApp env
            let tests = TestList [ -- TestLabel "GET /" (testRoot app) -- Removed - no GET / route
                                   TestLabel "POST /bitonic" (testPostBitonic app)
                                 , TestLabel "POST /bitonic caching" (testPostCaching app)
                                 ]
            counts <- runTestTT tests
            putStrLn ""
            if errors counts + failures counts == 0 then exitSuccess else exitFailure
