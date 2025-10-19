{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BitonicController (routes) where

import Web.Scotty
import BitonicModels (BitonicRequest(..))
import BitonicService (generateBitonic)
import AppContext (AppEnv, runApp)
import ApiError (ApiError(..), handleError, handleServiceError)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)

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
