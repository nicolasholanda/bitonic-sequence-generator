{-# LANGUAGE DeriveGeneric #-}

module BitonicService where

import BitonicSequence (bitonicArray)
import qualified BitonicRepository as Repo
import Database.Redis (Connection)
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

toRepoRequest :: BitonicRequest -> Repo.BitonicRequest
toRepoRequest req = Repo.BitonicRequest (n req) (l req) (r req)

generateBitonic :: Connection -> BitonicRequest -> IO BitonicResponse
generateBitonic conn req = do
    let repoReq = toRepoRequest req
    cached <- Repo.findBitonic conn repoReq
    
    case cached of
        Just sequence -> return $ BitonicResponse req sequence
        Nothing -> do
            let sequence = bitonicArray (n req) (l req) (r req)
            Repo.saveBitonic conn repoReq sequence
            return $ BitonicResponse req sequence
