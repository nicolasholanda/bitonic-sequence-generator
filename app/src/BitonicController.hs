{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : BitonicController
Description : HTTP routes for bitonic sequence endpoints
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module defines the HTTP API endpoints for generating bitonic sequences.
It handles request parsing, error handling, and response formatting.
-}
module BitonicController (routes) where

import Web.Scotty
import BitonicModels (BitonicRequest(..))
import BitonicService (generateBitonic)
import AppContext (AppEnv, runApp)
import ApiError (ApiError(..), handleError, handleServiceError)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)

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
routes :: AppEnv -> ScottyM ()
routes env = do
    post "/bitonic" $ do
        req <- (jsonData :: ActionM BitonicRequest) `Web.Scotty.catch` (\(_ :: SomeException) -> do
            handleError (InvalidRequest "Invalid JSON format or invalid field types")
            finish)
        result <- liftIO $ runApp env (generateBitonic req)
        case result of
            Right response -> json response
            Left err -> handleServiceError err
