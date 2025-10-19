{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiError where

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty (ActionM, status, json)
import Network.HTTP.Types.Status
import BitonicService (ServiceError(..))

data ErrorResponse = ErrorResponse
    { errorMessage :: String
    , errorCode :: String
    } deriving (Generic, Show)

instance ToJSON ErrorResponse

data ApiError 
    = InvalidRequest String
    | ServiceUnavailable String
    | InternalError String
    deriving (Show)

toErrorResponse :: ApiError -> ErrorResponse
toErrorResponse (InvalidRequest msg) = ErrorResponse msg "INVALID_REQUEST"
toErrorResponse (ServiceUnavailable msg) = ErrorResponse msg "SERVICE_UNAVAILABLE"
toErrorResponse (InternalError msg) = ErrorResponse msg "INTERNAL_ERROR"

toHttpStatus :: ApiError -> Status
toHttpStatus (InvalidRequest _) = status400
toHttpStatus (ServiceUnavailable _) = status503
toHttpStatus (InternalError _) = status500

handleError :: ApiError -> ActionM ()
handleError err = do
    status $ toHttpStatus err
    json $ toErrorResponse err

handleServiceError :: ServiceError -> ActionM ()
handleServiceError (InvalidParameters msg) = handleError $ InvalidRequest msg
handleServiceError (RedisError msg) = handleError $ ServiceUnavailable msg
handleServiceError (UnknownError msg) = handleError $ InternalError msg
