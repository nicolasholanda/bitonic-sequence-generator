{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BitonicService where

import BitonicSequence (bitonicArray)
import BitonicModels (BitonicRequest(..), BitonicResponse(..))
import qualified BitonicRepository as Repo
import qualified Database.Redis as Redis
import Control.Exception (try, SomeException)

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

generateBitonic :: Redis.Connection -> BitonicRequest -> IO (Either ServiceError BitonicResponse)
generateBitonic conn req = case validateRequest req of
    Left err -> return $ Left err
    Right validReq -> do
        result <- try (tryGenerate conn validReq)
        case result of
            Left (e :: SomeException) -> return $ Left $ RedisError $ "Redis operation failed: " ++ show e
            Right response -> return $ Right response
  where
    tryGenerate :: Redis.Connection -> BitonicRequest -> IO BitonicResponse
    tryGenerate c req = do
        cached <- Repo.findBitonic c req
        
        case cached of
            Just sequence -> return $ BitonicResponse req sequence
            Nothing -> do
                let sequence = bitonicArray (n req) (l req) (r req)
                Repo.saveBitonic c req sequence
                return $ BitonicResponse req sequence
