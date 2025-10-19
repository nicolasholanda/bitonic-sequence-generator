{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AppContext
Description : Application environment and monad definition
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module defines the application environment (dependencies) and the
application monad (AppM) using the ReaderT pattern. This eliminates the
need to pass the Redis connection through every function call.
-}
module AppContext 
    ( AppEnv(..)
    , AppM
    , initializeApp
    , runApp
    ) where

import qualified Database.Redis as Redis
import Control.Monad.Reader
import Control.Exception (try, SomeException)

{-|
Application environment containing all dependencies.

Currently contains only the Redis connection, but can be extended
to include configuration, database pools, loggers, etc.
-}
data AppEnv = AppEnv 
    { envRedisConn :: Redis.Connection  -- ^ Redis connection for caching
    }

{-|
Application monad using the ReaderT pattern.

All business logic and repository operations run in this monad,
which provides access to the application environment.
-}
type AppM = ReaderT AppEnv IO

{-|
Initialize the application environment.

Connects to Redis and creates the application environment.
Returns 'Left' with error message if connection fails.
-}
initializeApp :: IO (Either String AppEnv)
initializeApp = do
    putStrLn "Connecting to Redis..."
    connResult <- try (Redis.checkedConnect Redis.defaultConnectInfo) 
                   :: IO (Either SomeException Redis.Connection)
    case connResult of
        Left err -> return $ Left $ "Failed to connect to Redis: " ++ show err
        Right conn -> do
            putStrLn "Redis connected!"
            return $ Right $ AppEnv { envRedisConn = conn }

{-|
Run an AppM action with the provided environment.

This is the bridge between IO and AppM, typically called at the
HTTP handler boundary.
-}
runApp :: AppEnv -> AppM a -> IO a
runApp env action = runReaderT action env
