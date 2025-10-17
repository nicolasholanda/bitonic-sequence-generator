{-# LANGUAGE OverloadedStrings #-}

module BitonicRepository where

import BitonicModels (BitonicRequest(..))
import Database.Redis
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as BS

makeRedisKey :: BitonicRequest -> BS.ByteString
makeRedisKey req = BS.pack $ "bitonic:" ++ show (n req) ++ ":" ++ show (l req) ++ ":" ++ show (r req)

findBitonic :: Connection -> BitonicRequest -> IO (Maybe [Int])
findBitonic conn req = do
    result <- runRedis conn $ get (makeRedisKey req)
    case result of
        Right (Just value) -> return $ Aeson.decode $ fromStrict value
        _ -> return Nothing

saveBitonic :: Connection -> BitonicRequest -> [Int] -> IO ()
saveBitonic conn req sequence = do
    let key = makeRedisKey req
    let value = toStrict $ Aeson.encode sequence
    runRedis conn $ set key value
    return ()
