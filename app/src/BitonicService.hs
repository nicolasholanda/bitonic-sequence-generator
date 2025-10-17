{-# LANGUAGE DeriveGeneric #-}

module BitonicService where

import BitonicSequence (bitonicArray)
import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import qualified BitonicRepository as Repo
import Database.Redis (Connection)

generateBitonic :: Connection -> BitonicRequest -> IO BitonicResponse
generateBitonic conn req = do
    cached <- Repo.findBitonic conn req
    
    case cached of
        Just sequence -> return $ BitonicResponse req sequence
        Nothing -> do
            let sequence = bitonicArray (n req) (l req) (r req)
            Repo.saveBitonic conn req sequence
            return $ BitonicResponse req sequence
