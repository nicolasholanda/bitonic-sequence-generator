{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
module ApiError where

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty (ActionM, status, json)
import Network.HTTP.Types.Status
import BitonicService (ServiceError(..))

{-|
JSON error response format sent to clients.
-}
data ErrorResponse = ErrorResponse
    { errorMessage :: String  -- ^ Human-readable error message
    , errorCode :: String     -- ^ Machine-readable error code
    } deriving (Generic, Show)

instance ToJSON ErrorResponse

{-|
HTTP-level errors with status codes.
-}
data ApiError 
    = InvalidRequest String      -- ^ 400 Bad Request
    | ServiceUnavailable String  -- ^ 503 Service Unavailable
    | InternalError String       -- ^ 500 Internal Server Error
    deriving (Show)

{-|
Convert an ApiError to a JSON error response.
-}
toErrorResponse :: ApiError -> ErrorResponse
toErrorResponse (InvalidRequest msg) = ErrorResponse msg "INVALID_REQUEST"
toErrorResponse (ServiceUnavailable msg) = ErrorResponse msg "SERVICE_UNAVAILABLE"
toErrorResponse (InternalError msg) = ErrorResponse msg "INTERNAL_ERROR"

{-|
Map an ApiError to its HTTP status code.
-}
toHttpStatus :: ApiError -> Status
toHttpStatus (InvalidRequest _) = status400
toHttpStatus (ServiceUnavailable _) = status503
toHttpStatus (InternalError _) = status500

{-|
Send an error response to the client.

Sets the appropriate HTTP status code and returns a JSON error response.
-}
handleError :: ApiError -> ActionM ()
handleError err = do
    status $ toHttpStatus err
    json $ toErrorResponse err

{-|
Translate a service layer error to an HTTP error response.

This is the bridge between domain errors and HTTP errors, used by all controllers.
-}
handleServiceError :: ServiceError -> ActionM ()
handleServiceError (InvalidParameters msg) = handleError $ InvalidRequest msg
handleServiceError (RedisError msg) = handleError $ ServiceUnavailable msg
handleServiceError (UnknownError msg) = handleError $ InternalError msg
