{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BitonicService where

import BitonicSequence (bitonicArray)
import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import AppContext (AppM)
import qualified BitonicRepository as Repo
import Control.Exception (try, SomeException)
import Control.Monad.Reader

data ServiceError
    = InvalidParameters String
    | RedisError String
    | UnknownError String
    deriving (Show)

validateRequest :: BitonicRequest -> Either ServiceError BitonicRequest
validateRequest req
    | n req <= 0 = Left $ InvalidParameters "n must be positive"
    | l req < 0 = Left $ InvalidParameters "l must be positive"
    | r req <= l req = Left $ InvalidParameters "r must be greater than l"
    | otherwise = Right req

generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)
generateBitonic req = case validateRequest req of
    Left err -> return $ Left err
    Right validReq -> do
        result <- tryGenerate validReq
        return $ Right result
  where
    tryGenerate :: BitonicRequest -> AppM BitonicResponse
    tryGenerate req = do
        cached <- Repo.findBitonic req
        
        case cached of
            Just sequence -> return $ BitonicResponse req sequence
            Nothing -> do
                let sequence = bitonicArray (n req) (l req) (r req)
                Repo.saveBitonic req sequence
                return $ BitonicResponse req sequence
