{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import BitonicService (BitonicRequest(..), generateBitonic)
import qualified Database.Redis as Redis

main :: IO ()
main = do
    putStrLn "Connecting to Redis..."
    conn <- Redis.checkedConnect Redis.defaultConnectInfo
    putStrLn "Redis connected!"
    
    putStrLn "Starting Bitonic Sequence API on http://localhost:3000"
    scotty 3000 $ do
        get "/" $ text "Bitonic Sequence Generator API"
        
        post "/bitonic" $ do
            req <- jsonData :: ActionM BitonicRequest
            response <- liftIO $ generateBitonic conn req
            json response
