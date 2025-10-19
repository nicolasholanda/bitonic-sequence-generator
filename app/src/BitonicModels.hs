{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : BitonicModels
Description : Data transfer objects for bitonic sequence API
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module defines the request and response data types used throughout
the application. These types are shared between all layers (API, Service, Repository).
-}
module BitonicModels where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

{-|
Request parameters for generating a bitonic sequence.

The sequence will have @n@ elements from the range [@l@, @r@].
-}
data BitonicRequest = BitonicRequest
    { n :: Int  -- ^ Length of the sequence to generate
    , l :: Int  -- ^ Lower bound of the range (inclusive)
    , r :: Int  -- ^ Upper bound of the range (inclusive)
    } deriving (Generic, Show, Eq)

instance ToJSON BitonicRequest
instance FromJSON BitonicRequest

{-|
Response containing the generated bitonic sequence and the original request.
-}
data BitonicResponse = BitonicResponse
    { request :: BitonicRequest  -- ^ Original request parameters
    , result :: [Int]            -- ^ Generated bitonic sequence
    } deriving (Generic, Show)

instance ToJSON BitonicResponse
