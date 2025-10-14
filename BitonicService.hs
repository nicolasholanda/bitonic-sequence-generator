{-# LANGUAGE DeriveGeneric #-}

module BitonicService where

import BitonicSequence (bitonicArray)
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

generateBitonic :: BitonicRequest -> IO BitonicResponse
generateBitonic req = do
    let sequence = bitonicArray (n req) (l req) (r req)
    let response = BitonicResponse req sequence
    return response
