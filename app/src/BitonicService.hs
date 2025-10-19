{-# LANGUAGE DeriveGeneric #-}
-- ^ Enables automatic deriving of Generic instances
-- Generic is used for serialization and other generic programming

{-# LANGUAGE ScopedTypeVariables #-}
-- ^ Allows type annotations in local scopes (like in where clauses)
-- Needed for the (e :: SomeException) pattern match

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

-- Import the core algorithm
import BitonicSequence (bitonicArray)

-- Import our data transfer objects
import BitonicModels (BitonicRequest(..), BitonicResponse(..))

-- Import the application monad (AppM) that provides Redis connection
import AppContext (AppM)

-- Import repository functions (database access layer)
-- "qualified as Repo" means we must use Repo.function to call them
import qualified BitonicRepository as Repo

-- Import exception handling
import Control.Exception (try, SomeException)

-- Import Reader monad utilities
-- Reader allows passing environment (like Redis connection) implicitly
import Control.Monad.Reader

{-|
Service layer errors that can occur during bitonic sequence generation.
-}

-- Define error type using "data" (like an enum in Java)
-- This represents all possible errors in the service layer
data ServiceError
    = InvalidParameters String  -- ^ Input validation failed (400 Bad Request)
    | RedisError String         -- ^ Redis operation failed (503 Service Unavailable)
    | UnknownError String       -- ^ Unexpected error occurred (500 Internal Error)
    deriving (Show)
    -- ^ deriving Show allows converting to String (like toString())

{-|
Validate a bitonic sequence request.

Checks that:
* @n@ is positive
* @l@ is non-negative
* @r@ is greater than @l@

Returns 'Left' with error message if validation fails, 'Right' with the
validated request otherwise.
-}

-- Type signature: takes a request, returns Either error or valid request
-- Either is like Optional in Java, but can hold an error value
-- Left = error, Right = success
validateRequest :: BitonicRequest -> Either ServiceError BitonicRequest

-- Function implementation using guards (like if/else chain)
validateRequest req
    -- Check if n is non-positive
    -- | means "when this condition is true"
    -- $ is function application (avoids parentheses)
    | n req <= 0 = Left $ InvalidParameters "n must be positive"
    
    -- Check if l is negative
    | l req < 0 = Left $ InvalidParameters "l must be positive"
    
    -- Check if r is not greater than l
    | r req <= l req = Left $ InvalidParameters "r must be greater than l"
    
    -- If all validations pass, return Right (success)
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

-- Type signature:
-- Takes: BitonicRequest
-- Returns: AppM (Either ServiceError BitonicResponse)
--   AppM = our application monad (has access to Redis connection)
--   Either = can be Left (error) or Right (success)
generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)

-- Function implementation
-- "case...of" pattern matches on the validation result
generateBitonic req = case validateRequest req of
    -- If validation failed, immediately return the error
    -- "return" wraps a value in the monad (like return in Java, but different!)
    Left err -> return $ Left err
    
    -- If validation succeeded, continue with generation
    -- validReq is the validated request
    Right validReq -> do
        -- Try to generate the sequence (might use cache)
        -- <- executes the action and binds result to "result"
        result <- tryGenerate validReq
        
        -- Wrap the successful result in Right and return it
        return $ Right result
  where
    -- "where" defines helper functions (like private methods in Java)
    -- This helper function is only visible inside generateBitonic
    
    -- Helper to generate or retrieve from cache
    -- Type signature: takes request, returns AppM (monad) with response
    tryGenerate :: BitonicRequest -> AppM BitonicResponse
    
    tryGenerate req = do
        -- Look for cached result in Redis
        -- Repo.findBitonic runs in AppM monad (has access to Redis connection)
        cached <- Repo.findBitonic req
        
        -- Pattern match on Maybe (like Optional in Java)
        -- Maybe has two constructors: Nothing or Just value
        case cached of
            -- If we found it in cache, return it immediately
            -- Just sequence means cache hit
            Just sequence -> return $ BitonicResponse req sequence
            
            -- If not in cache, generate it
            -- Nothing means cache miss
            Nothing -> do
                -- Call the core algorithm to generate the sequence
                -- "let" introduces a local variable
                let sequence = bitonicArray (n req) (l req) (r req)
                
                -- Save the generated sequence to Redis for future requests
                Repo.saveBitonic req sequence
                
                -- Return the response with request and generated sequence
                return $ BitonicResponse req sequence
