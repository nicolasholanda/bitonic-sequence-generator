{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import BitonicModels (BitonicRequest(..))
import BitonicService (generateBitonic)
import qualified Database.Redis as Redis
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.Status as H

-- Handler used by `catch` when JSON parsing throws
badJson :: SomeException -> ActionM (Either String BitonicRequest)
badJson _ = return (Left "Invalid JSON payload")

main :: IO ()
main = do
    putStrLn "Connecting to Redis..."
    connResult <- try (Redis.checkedConnect Redis.defaultConnectInfo)
                   :: IO (Either SomeException Redis.Connection)
    case connResult of
        Left err -> do
            putStrLn $ "Failed to connect to Redis: " ++ show err
            putStrLn "Exiting."
        Right conn -> do
            putStrLn "Redis connected!"
            putStrLn "Starting Bitonic Sequence API on http://localhost:3000"

            scotty 3000 $ do
                get "/" $ text "Bitonic Sequence Generator API"

                post "/bitonic" $ do
                    -- Safely parse JSON; on failure, respond 400 with JSON error
                    eReq <- (Right <$> (jsonData :: ActionM BitonicRequest)) `catch` badJson

                    case eReq of
                        Left errMsg -> do
                            status H.status400
                            json $ object ["error" .= errMsg]

                        Right req -> do
                            -- Catch IO/Redis/service exceptions; on failure, respond 500
                            eResp <- liftIO (try (generateBitonic conn req))
                                       :: ActionM (Either SomeException BitonicResponse)
                            case eResp of
                                Left err -> do
                                    status H.status500
                                    json $ object ["error" .= TL.pack (show err)]
                                Right response ->
                                    json response
