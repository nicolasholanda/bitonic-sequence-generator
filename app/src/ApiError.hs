-- ============================================================================
-- LANGUAGE EXTENSIONS
-- ============================================================================
-- These are compiler flags that enable additional Haskell features.
-- Think of them like enabling experimental features in Java/TypeScript.

{-# LANGUAGE DeriveGeneric #-}
-- Enables automatic derivation of Generic instances.
-- Generic is a type class that provides a generic representation of data types.
-- This allows libraries like Aeson to automatically generate JSON encoders/decoders.
-- Without this, we'd have to write manual ToJSON instances.

{-# LANGUAGE OverloadedStrings #-}
-- Allows string literals to be polymorphic (work with multiple string types).
-- In Haskell, "hello" can be String, Text, or ByteString depending on context.
-- Without this, you'd need explicit conversions everywhere: fromString "hello"

-- ============================================================================
-- MODULE DOCUMENTATION (Haddock)
-- ============================================================================
-- This is Haddock documentation that generates nice HTML docs.
-- The pipe (|) character starts a Haddock comment block.
{-|
Module      : ApiError
Description : HTTP error handling and response formatting
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module provides consistent error handling across all API endpoints.
It translates domain errors (ServiceError) into HTTP errors with appropriate
status codes and JSON responses.
-}

-- ============================================================================
-- MODULE DECLARATION
-- ============================================================================
-- The 'module' keyword defines this file as a module named ApiError.
-- The 'where' keyword starts the module body (all the actual code).
-- Everything defined below 'where' is part of this module.
module ApiError where

-- ============================================================================
-- IMPORTS
-- ============================================================================
-- Import statements bring code from other modules into scope.
-- Think of this like 'import' in Java or 'import' in Python.

import GHC.Generics
-- GHC.Generics provides the Generic type class for automatic deriving.
-- This is part of the Glasgow Haskell Compiler (GHC) base libraries.

import Data.Aeson (ToJSON)
-- Data.Aeson is the JSON library for Haskell (like Jackson in Java).
-- We import only the ToJSON type class (selective import).
-- ToJSON defines how to convert a Haskell value to JSON.

import Web.Scotty (ActionM, status, json)
-- Web.Scotty is our web framework (like Express.js or Flask).
-- ActionM: The monad for web actions (handling HTTP requests/responses)
-- status: Function to set HTTP status code
-- json: Function to send JSON response

import Network.HTTP.Types.Status
-- Provides HTTP status code constants (status400, status500, etc.)
-- This is like HttpStatus enum in Spring Framework or http-status-codes in Node.js

import BitonicService (ServiceError(..))
-- Import ServiceError type from BitonicService module.
-- The (..) syntax imports the type AND all its constructors.
-- ServiceError has constructors: InvalidParameters, RedisError, UnknownError

-- ============================================================================
-- ERROR RESPONSE DATA TYPE
-- ============================================================================
-- This is the JSON structure sent to clients when errors occur.

{-|
JSON error response format sent to clients.
-}
-- Define a new data type called ErrorResponse.
-- This is like defining a class in Java or a struct in Go.
data ErrorResponse = ErrorResponse
-- The part before '=' is the TYPE NAME.
-- The part after '=' is the CONSTRUCTOR (also named ErrorResponse).
-- This is a "record syntax" constructor with named fields.

    { errorMessage :: String  -- ^ Human-readable error message
    -- Field name: errorMessage
    -- Field type: String
    -- The '^' in the comment makes this a Haddock field comment.
    -- Example: "Invalid parameter: n must be positive"
    
    , errorCode :: String     -- ^ Machine-readable error code
    -- Field name: errorCode
    -- Field type: String
    -- Example: "INVALID_REQUEST", "SERVICE_UNAVAILABLE"
    -- The comma at the START of this line separates fields (Haskell style).
    
    } deriving (Generic, Show)
    -- 'deriving' automatically generates instances (like implementing interfaces).
    -- Generic: Allows automatic ToJSON/FromJSON derivation.
    -- Show: Allows converting to string for debugging (like toString() in Java).

-- Automatically generate ToJSON instance for ErrorResponse.
-- This tells Aeson how to convert ErrorResponse to JSON.
-- Because we derived Generic above, this is just one line!
-- Without Generic, we'd need to write manual JSON encoding logic.
instance ToJSON ErrorResponse
-- This creates an instance of the ToJSON type class for ErrorResponse.
-- Now ErrorResponse can be used with json() function in Scotty.
-- Example JSON output: {"errorMessage": "Invalid input", "errorCode": "INVALID_REQUEST"}

-- ============================================================================
-- API ERROR DATA TYPE
-- ============================================================================
-- These are the HTTP-level errors our API can return.
-- This separates HTTP concerns from business logic errors.

{-|
HTTP-level errors with status codes.
-}
-- Define an enumeration-style data type (like enum in Java/TypeScript).
data ApiError 
-- Each constructor below represents a different error case.
-- Each constructor can hold data (the String parameter).

    = InvalidRequest String      -- ^ 400 Bad Request
    -- Constructor for client errors (bad input, malformed request).
    -- HTTP Status: 400 Bad Request
    -- Example: InvalidRequest "Missing required field: n"
    -- The String parameter holds the error message.
    
    | ServiceUnavailable String  -- ^ 503 Service Unavailable
    -- Constructor for service availability errors (Redis down, etc.).
    -- HTTP Status: 503 Service Unavailable
    -- Example: ServiceUnavailable "Redis connection failed"
    -- The pipe '|' separates alternative constructors (like 'or').
    
    | InternalError String       -- ^ 500 Internal Server Error
    -- Constructor for unexpected errors (bugs, unknown failures).
    -- HTTP Status: 500 Internal Server Error
    -- Example: InternalError "Unexpected exception occurred"
    
    deriving (Show)
    -- Only derive Show for debugging, not Generic (no JSON serialization needed).
    -- ApiError is internal; we convert it to ErrorResponse for JSON.

-- ============================================================================
-- CONVERT API ERROR TO JSON ERROR RESPONSE
-- ============================================================================
-- This function translates ApiError (internal) to ErrorResponse (JSON).

{-|
Convert an ApiError to a JSON error response.
-}
-- Function signature (type declaration):
-- Takes an ApiError → Returns an ErrorResponse
toErrorResponse :: ApiError -> ErrorResponse

-- Function body with pattern matching:
-- We define the function multiple times, once for each ApiError constructor.
-- Haskell will automatically choose the right version based on the input.
-- This is like a switch statement but more powerful and type-safe.

toErrorResponse (InvalidRequest msg) = ErrorResponse msg "INVALID_REQUEST"
-- Pattern: If input is (InvalidRequest msg)
-- Action: Create ErrorResponse with the message and code "INVALID_REQUEST"
-- 'msg' is a pattern variable that captures the String inside InvalidRequest.
-- Example: toErrorResponse (InvalidRequest "Bad input") 
--          → ErrorResponse "Bad input" "INVALID_REQUEST"

toErrorResponse (ServiceUnavailable msg) = ErrorResponse msg "SERVICE_UNAVAILABLE"
-- Pattern: If input is (ServiceUnavailable msg)
-- Action: Create ErrorResponse with the message and code "SERVICE_UNAVAILABLE"
-- Example: toErrorResponse (ServiceUnavailable "Redis down")
--          → ErrorResponse "Redis down" "SERVICE_UNAVAILABLE"

toErrorResponse (InternalError msg) = ErrorResponse msg "INTERNAL_ERROR"
-- Pattern: If input is (InternalError msg)
-- Action: Create ErrorResponse with the message and code "INTERNAL_ERROR"
-- Example: toErrorResponse (InternalError "Unknown error")
--          → ErrorResponse "Unknown error" "INTERNAL_ERROR"

-- ============================================================================
-- MAP API ERROR TO HTTP STATUS CODE
-- ============================================================================
-- This function determines which HTTP status code to return.

{-|
Map an ApiError to its HTTP status code.
-}
-- Function signature:
-- Takes an ApiError → Returns a Status (from Network.HTTP.Types.Status)
toHttpStatus :: ApiError -> Status

-- Pattern matching on ApiError constructors:
toHttpStatus (InvalidRequest _) = status400
-- Pattern: InvalidRequest with any message (we ignore it with underscore)
-- Action: Return status400 (HTTP 400 Bad Request)
-- The underscore '_' means "match anything but don't bind it to a variable"
-- We don't need the message here, only the constructor type matters.

toHttpStatus (ServiceUnavailable _) = status503
-- Pattern: ServiceUnavailable with any message
-- Action: Return status503 (HTTP 503 Service Unavailable)

toHttpStatus (InternalError _) = status500
-- Pattern: InternalError with any message
-- Action: Return status500 (HTTP 500 Internal Server Error)

-- ============================================================================
-- SEND ERROR RESPONSE TO CLIENT
-- ============================================================================
-- This function actually sends the HTTP error response.

{-|
Send an error response to the client.

Sets the appropriate HTTP status code and returns a JSON error response.
-}
-- Function signature:
-- Takes an ApiError → Returns an ActionM () action
-- ActionM is the Scotty web framework monad (like Express middleware in Node.js)
-- The () means "no meaningful return value" (like void in Java)
handleError :: ApiError -> ActionM ()

-- Function body using 'do' notation (monadic sequencing):
handleError err = do
-- 'err' is the parameter name (the ApiError we're handling)
-- 'do' starts a block of monadic actions (sequential operations)
-- Think of this like async/await in JavaScript - each line depends on the previous

    status $ toHttpStatus err
    -- Set the HTTP status code.
    -- The '$' operator means "apply function to everything on the right"
    -- Without '$': status (toHttpStatus err)
    -- With '$': status $ toHttpStatus err (cleaner, no parentheses needed)
    -- Steps:
    -- 1. Call toHttpStatus with 'err' → gets Status (400, 500, 503)
    -- 2. Pass that Status to 'status' function
    -- 3. Scotty sets the HTTP response status code
    
    json $ toErrorResponse err
    -- Send the JSON response body.
    -- Steps:
    -- 1. Call toErrorResponse with 'err' → gets ErrorResponse
    -- 2. Pass that ErrorResponse to 'json' function
    -- 3. Scotty serializes to JSON and sends as response body
    -- Example output: {"errorMessage": "Invalid input", "errorCode": "INVALID_REQUEST"}

-- ============================================================================
-- TRANSLATE SERVICE ERRORS TO HTTP ERRORS
-- ============================================================================
-- This is the bridge between domain layer and HTTP layer.

{-|
Translate a service layer error to an HTTP error response.

This is the bridge between domain errors and HTTP errors, used by all controllers.
-}
-- Function signature:
-- Takes a ServiceError (from BitonicService) → Returns an ActionM () action
handleServiceError :: ServiceError -> ActionM ()

-- Pattern matching on ServiceError constructors:
-- ServiceError has three constructors: InvalidParameters, RedisError, UnknownError
-- We map each to the appropriate ApiError, then use handleError.

handleServiceError (InvalidParameters msg) = handleError $ InvalidRequest msg
-- Pattern: If ServiceError is InvalidParameters
-- Action: Convert to InvalidRequest (400 Bad Request) and handle it
-- Flow: ServiceError (InvalidParameters "bad input")
--       → ApiError (InvalidRequest "bad input")
--       → HTTP 400 + {"errorMessage": "bad input", "errorCode": "INVALID_REQUEST"}

handleServiceError (RedisError msg) = handleError $ ServiceUnavailable msg
-- Pattern: If ServiceError is RedisError
-- Action: Convert to ServiceUnavailable (503) and handle it
-- This maps Redis failures to "service temporarily unavailable"

handleServiceError (UnknownError msg) = handleError $ InternalError msg
-- Pattern: If ServiceError is UnknownError
-- Action: Convert to InternalError (500) and handle it
-- This maps unexpected errors to "internal server error"

-- ============================================================================
-- SUMMARY OF ERROR FLOW
-- ============================================================================
-- 1. Business layer detects error → creates ServiceError
-- 2. Controller catches ServiceError → calls handleServiceError
-- 3. handleServiceError maps ServiceError → ApiError
-- 4. handleError maps ApiError → ErrorResponse + HTTP status
-- 5. Client receives HTTP status + JSON error body
--
-- Example flow:
-- InvalidParameters "n must be > 0"  (BitonicService)
--   → InvalidRequest "n must be > 0"   (ApiError - this module)
--   → HTTP 400 + {"errorMessage": "n must be > 0", "errorCode": "INVALID_REQUEST"}
-- ============================================================================
