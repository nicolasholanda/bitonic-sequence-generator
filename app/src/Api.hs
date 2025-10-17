{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import BitonicModels (BitonicRequest(..))
import BitonicService (generateBitonic, ServiceError(..))
import ApiError (ApiError(..), handleError)
import qualified Database.Redis as Redis
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)

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
                    req <- (jsonData :: ActionM BitonicRequest) `Web.Scotty.catch` (\(_ :: SomeException) -> do
                        handleError (InvalidRequest "Invalid JSON format or invalid field types")
                        finish)
                    result <- liftIO $ generateBitonic conn req
                    case result of
                        Right response -> json response
                        Left err -> handleServiceError err

handleServiceError :: ServiceError -> ActionM ()
handleServiceError (InvalidParameters msg) = handleError $ InvalidRequest msg
handleServiceError (RedisError msg) = handleError $ ServiceUnavailable msg
handleServiceError (UnknownError msg) = handleError $ InternalError msg
