{-# LANGUAGE OverloadedStrings #-}
-- ^ Allows string literals to be polymorphic (work as ByteString, Text, etc.)

{-|
Module      : BitonicRepository
Description : Data access layer for Redis operations
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module handles persistence of bitonic sequences in Redis.
It provides functions to cache and retrieve computed sequences.
-}
module BitonicRepository where

-- Import our request data type
import BitonicModels (BitonicRequest(..))

-- Import application monad and environment
-- AppM = our monad that has Redis connection available
-- AppEnv = the environment record that contains the Redis connection
import AppContext (AppM, AppEnv(..))

-- Import Redis client library
-- This provides functions to interact with Redis database
import Database.Redis

-- Import JSON serialization library (converts Haskell ↔ JSON)
-- "qualified as Aeson" means we use Aeson.encode, Aeson.decode, etc.
import qualified Data.Aeson as Aeson

-- Import ByteString conversion functions
-- toStrict: lazy ByteString → strict ByteString
-- fromStrict: strict ByteString → lazy ByteString
-- (Redis uses strict, Aeson uses lazy)
import Data.ByteString.Lazy (toStrict, fromStrict)

-- Import strict ByteString with Char8 encoding
-- "qualified as BS" means we use BS.pack, BS.unpack, etc.
import qualified Data.ByteString.Char8 as BS

-- Import Reader monad utilities
-- Provides "asks" to access environment fields
import Control.Monad.Reader

{-|
Generate a Redis key for a bitonic sequence request.

The key format is: @bitonic:n:l:r@

Example: @bitonic:5:3:10@
-}

-- Type signature: takes request, returns ByteString (Redis key)
-- BS.ByteString is an efficient byte array (better than String for Redis)
makeRedisKey :: BitonicRequest -> BS.ByteString

-- Implementation
makeRedisKey req = BS.pack $ "bitonic:" ++ show (n req) ++ ":" ++ show (l req) ++ ":" ++ show (r req)
    -- Breaking it down:
    -- show (n req) converts integer to String (like .toString())
    -- ++ concatenates strings: "bitonic:" ++ "5" ++ ":" ++ "3" ++ ":" ++ "10"
    -- Result: "bitonic:5:3:10"
    -- BS.pack converts String to ByteString
    -- $ is function application (avoids parentheses)

{-|
Find a cached bitonic sequence in Redis.

Returns 'Just' the sequence if found in cache, 'Nothing' otherwise.
-}

-- Type signature:
-- Takes: BitonicRequest
-- Returns: AppM (Maybe [Int])
--   AppM = runs in application monad (has access to Redis)
--   Maybe = might be Nothing (not found) or Just [Int] (found)
findBitonic :: BitonicRequest -> AppM (Maybe [Int])

-- Implementation
-- "do" notation for monadic code (sequential actions)
findBitonic req = do
    -- Get the Redis connection from environment
    -- "asks" is like "ask" but lets you select a field
    -- envRedisConn is a field accessor function
    -- This is the magic of Reader monad - no need to pass conn manually!
    conn <- asks envRedisConn
    
    -- Perform Redis GET operation
    -- liftIO lifts an IO action into AppM monad
    -- runRedis executes Redis commands
    -- get fetches value by key
    -- $ chains: runRedis conn (get (makeRedisKey req))
    result <- liftIO $ runRedis conn $ get (makeRedisKey req)
    
    -- Pattern match on the result
    -- Redis returns: Either Reply (Maybe ByteString)
    --   Either = command might fail (network error, etc.)
    --   Maybe = key might not exist
    case result of
        -- If successful AND value exists
        -- Right means Redis command succeeded
        -- Just value means key was found
        Right (Just value) -> return $ Aeson.decode $ fromStrict value
            -- fromStrict: strict ByteString → lazy ByteString
            -- Aeson.decode: lazy ByteString → Maybe [Int]
            -- Parses JSON "[1,2,3]" into Haskell list [1,2,3]
            -- return wraps in AppM monad
        
        -- Any other case (error or not found)
        -- _ is wildcard pattern (matches anything, value ignored)
        _ -> return Nothing
            -- return Nothing in AppM monad

{-|
Save a bitonic sequence to Redis cache.

Stores the sequence as JSON using the request parameters as the key.
-}

-- Type signature:
-- Takes: BitonicRequest and [Int] (the sequence)
-- Returns: AppM ()
--   AppM = runs in application monad
--   () = no return value (like void in Java)
saveBitonic :: BitonicRequest -> [Int] -> AppM ()

-- Implementation
saveBitonic req sequence = do
    -- Get Redis connection from environment
    conn <- asks envRedisConn
    
    -- Generate the Redis key
    -- "let" introduces local variables (no <- because it's pure, not monadic)
    let key = makeRedisKey req
    
    -- Convert sequence to JSON ByteString
    let value = toStrict $ Aeson.encode sequence
        -- Aeson.encode: [Int] → lazy ByteString (JSON)
        -- Example: [1,2,3] → "[1,2,3]" as JSON
        -- toStrict: lazy ByteString → strict ByteString (for Redis)
        -- $ is function application
    
    -- Store in Redis
    -- liftIO: lift IO action into AppM
    -- runRedis: execute Redis command
    -- set: SET key value (stores in Redis)
    liftIO $ runRedis conn $ set key value
    
    -- Return unit () - indicates success
    return ()
