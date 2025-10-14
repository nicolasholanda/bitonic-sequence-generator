{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import BitonicService (BitonicRequest(..), generateBitonic)
import GHC.Generics

main :: IO ()
main = do
    putStrLn "Starting Bitonic Sequence API"
    scotty 3000 $ do
        get "/" $ text "Bitonic Sequence Generator API"
        
        post "/bitonic" $ do
            req <- jsonData :: ActionM BitonicRequest
            response <- liftIO $ generateBitonic req
            json response
