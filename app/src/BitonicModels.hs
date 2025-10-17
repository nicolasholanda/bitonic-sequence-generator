{-# LANGUAGE DeriveGeneric #-}

module BitonicModels where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data BitonicRequest = BitonicRequest
    { n :: Int
    , l :: Int
    , r :: Int
    } deriving (Generic, Show, Eq)

instance ToJSON BitonicRequest
instance FromJSON BitonicRequest

data BitonicResponse = BitonicResponse
    { request :: BitonicRequest
    , result :: [Int]
    } deriving (Generic, Show)

instance ToJSON BitonicResponse
