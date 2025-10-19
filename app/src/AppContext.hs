{-# LANGUAGE OverloadedStrings #-}

module AppContext 
    ( AppEnv(..)
    , AppM
    , initializeApp
    , runApp
    ) where

import qualified Database.Redis as Redis
import Control.Monad.Reader
import Control.Exception (try, SomeException)

data AppEnv = AppEnv 
    { envRedisConn :: Redis.Connection
    }

type AppM = ReaderT AppEnv IO

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

-- | Run an AppM action with the environment
runApp :: AppEnv -> AppM a -> IO a
runApp env action = runReaderT action env
