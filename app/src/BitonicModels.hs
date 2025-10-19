{-# LANGUAGE DeriveGeneric #-}
-- ^ This is a "language pragma" - enables the DeriveGeneric extension
-- DeriveGeneric allows automatic derivation of Generic instances
-- (needed for automatic JSON serialization)

{-|
Module      : BitonicModels
Description : Data transfer objects for bitonic sequence API
Copyright   : (c) 2025
License     : MIT
Stability   : experimental

This module defines the request and response data types used throughout
the application. These types are shared between all layers (API, Service, Repository).
-}

-- Module declaration - only exports these types and their constructors
-- The (..) means "export the type and all its constructors/fields"
module BitonicModels where

-- Import Generic from GHC.Generics
-- Generic is used for automatic JSON conversion
import GHC.Generics

-- Import JSON serialization type classes from Aeson library
-- ToJSON: convert Haskell data to JSON
-- FromJSON: convert JSON to Haskell data
import Data.Aeson (ToJSON, FromJSON)

{-|
Request parameters for generating a bitonic sequence.

The sequence will have @n@ elements from the range [@l@, @r@].
-}

-- "data" keyword defines a new data type (like a Java class)
-- BitonicRequest is the type name
-- = BitonicRequest is the constructor (in this case, same name as the type)
-- Record syntax: { fieldName :: Type }
data BitonicRequest = BitonicRequest
    { n :: Int  -- ^ Length of the sequence to generate
    , l :: Int  -- ^ Lower bound of the range (inclusive)
    , r :: Int  -- ^ Upper bound of the range (inclusive)
    } deriving (Generic, Show, Eq)
    -- ^ "deriving" automatically generates instances of these type classes:
    -- Generic: for automatic JSON conversion
    -- Show: converts to String (like toString() in Java)
    -- Eq: allows == comparison

-- "instance" declares that BitonicRequest implements ToJSON type class
-- Automatically derived thanks to Generic - no implementation needed!
-- This allows converting BitonicRequest to JSON
instance ToJSON BitonicRequest

-- Automatically derived thanks to Generic
-- This allows parsing JSON into BitonicRequest
instance FromJSON BitonicRequest

{-|
Response containing the generated bitonic sequence and the original request.
-}

-- Another data type for the response
data BitonicResponse = BitonicResponse
    { request :: BitonicRequest  -- ^ Original request parameters (nested object)
    , result :: [Int]            -- ^ Generated bitonic sequence (list of integers)
    } deriving (Generic, Show)
    -- Note: No Eq here because we don't need to compare responses

-- Allows converting BitonicResponse to JSON
-- The response will look like: {"request": {...}, "result": [1,2,3]}
instance ToJSON BitonicResponse
