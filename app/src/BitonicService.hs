{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : BitonicService
Description : Business logic layer for bitonic sequence generation
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module contains the business logic for generating bitonic sequences.
It handles request validation, caching, and orchestrates calls to the
repository and core algorithm.
-}
module BitonicService where

import BitonicSequence (bitonicArray)
import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import AppContext (AppM)
import qualified BitonicRepository as Repo
import Control.Exception (try, SomeException)
import Control.Monad.Reader

{-|
Service layer errors that can occur during bitonic sequence generation.
-}
data ServiceError
    = InvalidParameters String  -- ^ Input validation failed
    | RedisError String         -- ^ Redis operation failed
    | UnknownError String       -- ^ Unexpected error occurred
    deriving (Show)

{-|
Validate a bitonic sequence request.

Checks that:
* @n@ is positive
* @l@ is non-negative
* @r@ is greater than @l@

Returns 'Left' with error message if validation fails, 'Right' with the
validated request otherwise.
-}
validateRequest :: BitonicRequest -> Either ServiceError BitonicRequest
validateRequest req
    | n req <= 0 = Left $ InvalidParameters "n must be positive"
    | l req < 0 = Left $ InvalidParameters "l must be positive"
    | r req <= l req = Left $ InvalidParameters "r must be greater than l"
    | otherwise = Right req

{-|
Generate a bitonic sequence with caching.

This function:

1. Validates the request parameters
2. Checks if the result is cached in Redis
3. If cached, returns the cached result
4. If not cached, generates the sequence and stores it in Redis
5. Returns the response

Returns 'Left' with a 'ServiceError' if validation fails or a Redis error occurs.
-}
generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)
generateBitonic req = case validateRequest req of
    Left err -> return $ Left err
    Right validReq -> do
        result <- tryGenerate validReq
        return $ Right result
  where
    tryGenerate :: BitonicRequest -> AppM BitonicResponse
    tryGenerate req = do
        cached <- Repo.findBitonic req
        
        case cached of
            Just sequence -> return $ BitonicResponse req sequence
            Nothing -> do
                let sequence = bitonicArray (n req) (l req) (r req)
                Repo.saveBitonic req sequence
                return $ BitonicResponse req sequence
