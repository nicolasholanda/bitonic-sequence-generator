-- ============================================================================
-- LANGUAGE EXTENSIONS
-- ============================================================================
-- These compiler flags enable additional Haskell features.

{-# LANGUAGE OverloadedStrings #-}
-- Allows string literals to be polymorphic.
-- "hello" can be String, Text, or ByteString depending on context.
-- Essential for web frameworks where route paths can be different string types.

{-# LANGUAGE ScopedTypeVariables #-}
-- Allows type annotations inside function bodies to reference type variables from signatures.
-- Used here for the explicit type annotation: (jsonData :: ActionM BitonicRequest)
-- This tells the compiler exactly what type we expect from jsonData.

-- ============================================================================
-- MODULE DOCUMENTATION (Haddock)
-- ============================================================================
{-|
Module      : BitonicController
Description : HTTP routes for bitonic sequence endpoints
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module defines the HTTP API endpoints for generating bitonic sequences.
It handles request parsing, error handling, and response formatting.
-}

-- ============================================================================
-- MODULE DECLARATION
-- ============================================================================
-- This file is a module named BitonicController.
-- We export only the 'routes' function (selective export).
-- Other modules can import BitonicController and use routes, but not internal helpers.
module BitonicController (routes) where
-- The part in parentheses lists what's exported (public API).
-- Without parentheses, everything would be exported.

-- ============================================================================
-- IMPORTS
-- ============================================================================

import Web.Scotty
-- Scotty is our web framework (like Express.js, Flask, or Sinatra).
-- This imports everything from Web.Scotty:
--   - ScottyM: Monad for defining routes
--   - ActionM: Monad for handling HTTP requests/responses
--   - post, get: Functions to define HTTP routes
--   - jsonData: Parse request body as JSON
--   - json: Send JSON response
--   - catch: Exception handling for ActionM
--   - finish: Stop request processing immediately

import BitonicModels (BitonicRequest(..))
-- Import the BitonicRequest data type from BitonicModels module.
-- The (..) syntax imports the type AND its constructors/fields.
-- BitonicRequest is what we expect in POST request body.

import BitonicService (generateBitonic)
-- Import the business logic function that generates bitonic sequences.
-- generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)

import AppContext (AppEnv, runApp)
-- Import application context types and runner:
--   - AppEnv: The environment record containing Redis connection
--   - runApp: Function to execute AppM actions in IO

import ApiError (ApiError(..), handleError, handleServiceError)
-- Import error handling utilities:
--   - ApiError(..): The ApiError type and all its constructors
--   - handleError: Send ApiError as HTTP response
--   - handleServiceError: Convert ServiceError to HTTP response

import Control.Exception (SomeException)
-- SomeException is the root type for all exceptions in Haskell.
-- Like Throwable in Java or Error in JavaScript.
-- Used to catch any exception during JSON parsing.

import Control.Monad.IO.Class (liftIO)
-- liftIO is a function that lifts IO actions into other monads.
-- Type: liftIO :: IO a -> m a (where m is any MonadIO instance)
-- Here we use it to run IO actions inside ActionM.
-- Like wrapping a Promise in async/await or running blocking code in async context.

-- ============================================================================
-- ROUTE DEFINITIONS
-- ============================================================================
-- This function defines all HTTP endpoints for our API.

{-|
Define all HTTP routes for the bitonic sequence API.

Currently provides:

* @POST \/bitonic@ - Generate a bitonic sequence

Example request:

@
{
  "n": 5,
  "l": 3,
  "r": 10
}
@

Example response:

@
{
  "request": {"n": 5, "l": 3, "r": 10},
  "result": [9, 10, 9, 8, 7]
}
@
-}
-- Function signature:
-- Takes AppEnv (application environment with Redis connection)
-- Returns ScottyM () (Scotty monad for defining routes)
-- ScottyM is like the Router in Express.js - it builds route definitions
-- The () means "no meaningful return value" (like void in Java)
routes :: AppEnv -> ScottyM ()

-- Function body using 'do' notation:
routes env = do
-- 'env' is the parameter - the application environment (AppEnv record)
-- 'do' starts a block of monadic actions (sequential route definitions)
-- In ScottyM, each statement defines a new route

    -- ========================================================================
    -- POST /bitonic - Generate bitonic sequence endpoint
    -- ========================================================================
    post "/bitonic" $ do
    -- 'post' defines a POST endpoint.
    -- "/bitonic" is the route path (matches POST requests to /bitonic)
    -- The '$' operator means "apply function to everything on the right"
    -- The 'do' block after '$' contains the request handler logic
    -- This is like: app.post("/bitonic", async (req, res) => { ... }) in Express
    
        -- ====================================================================
        -- PARSE JSON REQUEST BODY WITH ERROR HANDLING
        -- ====================================================================
        -- We use pattern binding with error recovery:
        req <- (jsonData :: ActionM BitonicRequest) `Web.Scotty.catch` (\(_ :: SomeException) -> do
        -- This is complex, so let's break it down piece by piece:
        
        -- Part 1: (jsonData :: ActionM BitonicRequest)
        --   - jsonData is a Scotty function that parses request body as JSON
        --   - The :: annotation specifies the expected type (BitonicRequest)
        --   - Without ScopedTypeVariables extension, this wouldn't compile
        --   - This is like req.body in Express with type checking
        --   - If JSON is invalid or doesn't match BitonicRequest structure, throws exception
        
        -- Part 2: `Web.Scotty.catch`
        --   - Backticks turn a function into an infix operator
        --   - catch :: ActionM a -> (SomeException -> ActionM a) -> ActionM a
        --   - This catches any exception from jsonData
        --   - Like try/catch in Java or .catch() in JavaScript
        
        -- Part 3: (\(_ :: SomeException) -> do ...)
        --   - This is a lambda function (anonymous function)
        --   - The backslash '\' starts a lambda (like λ in lambda calculus)
        --   - (_ :: SomeException) is the parameter pattern
        --   - The underscore means "ignore the exception details"
        --   - The :: SomeException is a type annotation
        --   - Like: (exception: Error) => { ... } in TypeScript
        
            handleError (InvalidRequest "Invalid JSON format or invalid field types")
            -- If JSON parsing fails, send 400 Bad Request error.
            -- handleError sets HTTP status to 400 and sends JSON error response.
            -- Example error response:
            -- {"errorMessage": "Invalid JSON...", "errorCode": "INVALID_REQUEST"}
            
            finish)
            -- 'finish' immediately stops request processing.
            -- Like res.end() in Express or return in a controller.
            -- Without finish, execution would continue and 'req' would be undefined.
        
        -- Part 4: req <-
        --   - The '<-' operator extracts the value from the monadic action
        --   - If jsonData succeeds: req gets the BitonicRequest value
        --   - If jsonData fails: catch handler runs, error sent, finish stops execution
        --   - Like 'await' in async/await: req = await parseJson(body)
        
        -- ====================================================================
        -- CALL BUSINESS LOGIC
        -- ====================================================================
        result <- liftIO $ runApp env (generateBitonic req)
        -- This line is the bridge between HTTP layer and business logic.
        -- Let's break it down:
        
        -- Part 1: generateBitonic req
        --   - Call the business logic function with the parsed request
        --   - Type: BitonicRequest -> AppM (Either ServiceError BitonicResponse)
        --   - AppM is our custom monad (ReaderT AppEnv IO)
        --   - Returns Either (success or error)
        
        -- Part 2: runApp env (...)
        --   - runApp executes an AppM action in IO
        --   - Type: AppEnv -> AppM a -> IO a
        --   - Provides the environment (Redis connection) to the action
        --   - Unwraps the ReaderT to plain IO
        --   - Like running a function that needs dependency injection
        
        -- Part 3: liftIO $ ...
        --   - liftIO lifts an IO action into ActionM
        --   - Type: IO a -> ActionM a
        --   - Scotty's ActionM can run IO actions via liftIO
        --   - Like making an async call from within an Express route handler
        
        -- Part 4: result <-
        --   - Extract the value from the ActionM monad
        --   - result :: Either ServiceError BitonicResponse
        --   - If Left: contains ServiceError (business logic error)
        --   - If Right: contains BitonicResponse (success)
        
        -- ====================================================================
        -- HANDLE RESULT (SUCCESS OR ERROR)
        -- ====================================================================
        case result of
        -- Pattern matching on Either (like switch on discriminated union in TypeScript)
        -- Either has two constructors: Left (error) and Right (success)
        -- This is idiomatic functional error handling (no exceptions)
        
            Right response -> json response
            -- Pattern: If result is Right response
            --   - response :: BitonicResponse (the success value)
            --   - json sends it as JSON with HTTP 200 OK
            --   - Example: {"request": {...}, "result": [9, 10, 9, 8, 7]}
            --   - Like res.json(response) in Express
            
            Left err -> handleServiceError err
            -- Pattern: If result is Left err
            --   - err :: ServiceError (the error value)
            --   - handleServiceError converts it to HTTP error
            --   - Maps InvalidParameters → 400, RedisError → 503, UnknownError → 500
            --   - Sends appropriate status code + JSON error response
            --   - Like res.status(500).json({error: err.message}) in Express

-- ============================================================================
-- SUMMARY OF REQUEST FLOW
-- ============================================================================
-- 1. Client sends POST /bitonic with JSON body
-- 2. Scotty routes to this handler
-- 3. jsonData parses body as BitonicRequest (or throws exception)
-- 4. catch handler catches parse errors → 400 Bad Request
-- 5. generateBitonic validates and processes (in AppM with Redis access)
-- 6. runApp executes AppM in IO (provides Redis connection)
-- 7. liftIO bridges IO to ActionM (Scotty context)
-- 8. Pattern match on Either:
--    - Right response → json response (200 OK)
--    - Left err → handleServiceError err (400/500/503)
-- 9. Client receives HTTP status + JSON response
--
-- Example success flow:
-- POST {"n": 5, "l": 3, "r": 10}
--   → BitonicRequest {n=5, l=3, r=10}
--   → generateBitonic validates, checks cache, generates
--   → Right (BitonicResponse {...})
--   → HTTP 200 + {"request": {...}, "result": [9,10,9,8,7]}
--
-- Example error flow:
-- POST {"n": -5, "l": 3, "r": 10}
--   → BitonicRequest {n=-5, l=3, r=10}
--   → generateBitonic validates
--   → Left (InvalidParameters "n must be > 0")
--   → handleServiceError (InvalidParameters "...")
--   → HTTP 400 + {"errorMessage": "n must be > 0", "errorCode": "INVALID_REQUEST"}
-- ============================================================================
