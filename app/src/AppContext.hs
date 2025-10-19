{-# LANGUAGE OverloadedStrings #-}
-- ^ Allows string literals to be treated as different text types

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

-- Module declaration with explicit exports
-- Only these four items are visible to other modules
module AppContext 
    ( AppEnv(..)      -- Export type and all its constructors/fields
    , AppM            -- Export the type alias
    , initializeApp   -- Export initialization function
    , runApp          -- Export runner function
    ) where

-- Import Redis client library
-- "qualified as Redis" means we must use Redis.function
import qualified Database.Redis as Redis

-- Import Reader monad transformer
-- ReaderT allows carrying environment through function calls implicitly
-- Think of it as "dependency injection" built into the type system
import Control.Monad.Reader

-- Import exception handling
-- try: safely execute action that might throw exception
-- SomeException: top-level exception type (catches all exceptions)
import Control.Exception (try, SomeException)

{-|
Application environment containing all dependencies.

Currently contains only the Redis connection, but can be extended
to include configuration, database pools, loggers, etc.
-}

-- Define a record type (like a Java class with fields)
-- This holds all the "dependencies" our application needs
data AppEnv = AppEnv 
    { envRedisConn :: Redis.Connection  -- ^ Field: Redis connection
    }
    -- Record syntax creates both:
    -- 1. A constructor: AppEnv
    -- 2. A field accessor function: envRedisConn :: AppEnv -> Redis.Connection
    
    -- Note: No deriving clause - we don't need Show, Eq for connections

{-|
Application monad using the ReaderT pattern.

All business logic and repository operations run in this monad,
which provides access to the application environment.
-}

-- "type" creates a type alias (like typedef in C)
-- This is NOT a new type, just a convenient name
-- 
-- Breaking down "ReaderT AppEnv IO":
--   ReaderT = Reader monad transformer
--   AppEnv = the environment type we're carrying
--   IO = the base monad (allows IO operations)
--
-- Think of AppM as: "IO operations with automatic access to AppEnv"
-- Functions in AppM can:
--   1. Do IO (read files, network calls, etc.)
--   2. Access AppEnv (get Redis connection) via "ask" or "asks"
type AppM = ReaderT AppEnv IO

{-|
Initialize the application environment.

Connects to Redis and creates the application environment.
Returns 'Left' with error message if connection fails.
-}

-- Type signature:
-- Returns: IO (Either String AppEnv)
--   IO = performs IO operations
--   Either String AppEnv = 
--     Left String = error message if failed
--     Right AppEnv = environment if successful
initializeApp :: IO (Either String AppEnv)

-- Implementation
-- "do" notation for IO actions
initializeApp = do
    -- Print status message
    putStrLn "Connecting to Redis..."
    
    -- Try to connect to Redis (might throw exception)
    -- try wraps the action to catch any exceptions
    -- <- binds the result to connResult
    connResult <- try (Redis.checkedConnect Redis.defaultConnectInfo) 
                   :: IO (Either SomeException Redis.Connection)
    -- The :: annotation specifies the return type
    -- checkedConnect throws exception if Redis is unavailable
    -- try catches it and wraps in Either:
    --   Left exception = connection failed
    --   Right connection = success
    
    -- Pattern match on Either
    case connResult of
        -- If connection failed (exception was thrown)
        Left err -> return $ Left $ "Failed to connect to Redis: " ++ show err
            -- show: converts exception to String
            -- ++: concatenates strings
            -- Left: wraps error message in Either
            -- return: wraps in IO monad
        
        -- If connection succeeded
        Right conn -> do
            putStrLn "Redis connected!"
            
            -- Create the AppEnv record
            -- Record syntax: Type { field = value }
            return $ Right $ AppEnv { envRedisConn = conn }
            -- AppEnv { envRedisConn = conn } creates the environment
            -- Right wraps it as success
            -- return wraps in IO monad

{-|
Run an AppM action with the provided environment.

This is the bridge between IO and AppM, typically called at the
HTTP handler boundary.
-}

-- Type signature:
-- Takes: AppEnv (the environment) and AppM a (an action that returns 'a')
-- Returns: IO a (unwraps AppM to plain IO)
-- 
-- The 'a' is a type variable (like generics in Java: <T>)
-- This works for any return type
runApp :: AppEnv -> AppM a -> IO a

-- Implementation
runApp env action = runReaderT action env
    -- runReaderT is provided by the Reader monad library
    -- It "runs" a ReaderT action by providing the environment
    -- 
    -- Before: action :: ReaderT AppEnv IO a (needs environment)
    -- After: runReaderT action env :: IO a (environment provided)
    --
    -- This is how we convert from AppM back to IO
    -- Typically called in HTTP handlers to inject dependencies
