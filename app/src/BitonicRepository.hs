{-# LANGUAGE OverloadedStrings #-}

module BitonicRepository where

import BitonicModels (BitonicRequest(..))
import AppContext (AppM, AppEnv(..))
import Database.Redis
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader

makeRedisKey :: BitonicRequest -> BS.ByteString
makeRedisKey req = BS.pack $ "bitonic:" ++ show (n req) ++ ":" ++ show (l req) ++ ":" ++ show (r req)

findBitonic :: BitonicRequest -> AppM (Maybe [Int])
findBitonic req = do
    conn <- asks envRedisConn
    result <- liftIO $ runRedis conn $ get (makeRedisKey req)
    case result of
        Right (Just value) -> return $ Aeson.decode $ fromStrict value
        _ -> return Nothing

saveBitonic :: BitonicRequest -> [Int] -> AppM ()
saveBitonic req sequence = do
    conn <- asks envRedisConn
    let key = makeRedisKey req
    let value = toStrict $ Aeson.encode sequence
    liftIO $ runRedis conn $ set key value
    return ()
