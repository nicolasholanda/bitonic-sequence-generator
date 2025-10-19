{-# LANGUAGE OverloadedStrings #-}

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

import BitonicModels (BitonicRequest(..))
import AppContext (AppM, AppEnv(..))
import Database.Redis
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader

{-|
Generate a Redis key for a bitonic sequence request.

The key format is: @bitonic:n:l:r@

Example: @bitonic:5:3:10@
-}
makeRedisKey :: BitonicRequest -> BS.ByteString
makeRedisKey req = BS.pack $ "bitonic:" ++ show (n req) ++ ":" ++ show (l req) ++ ":" ++ show (r req)

{-|
Find a cached bitonic sequence in Redis.

Returns 'Just' the sequence if found in cache, 'Nothing' otherwise.
-}
findBitonic :: BitonicRequest -> AppM (Maybe [Int])
findBitonic req = do
    conn <- asks envRedisConn
    result <- liftIO $ runRedis conn $ get (makeRedisKey req)
    case result of
        Right (Just value) -> return $ Aeson.decode $ fromStrict value
        _ -> return Nothing

{-|
Save a bitonic sequence to Redis cache.

Stores the sequence as JSON using the request parameters as the key.
-}
saveBitonic :: BitonicRequest -> [Int] -> AppM ()
saveBitonic req sequence = do
    conn <- asks envRedisConn
    let key = makeRedisKey req
    let value = toStrict $ Aeson.encode sequence
    liftIO $ runRedis conn $ set key value
    return ()
