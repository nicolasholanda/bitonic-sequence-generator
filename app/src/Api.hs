module Main where

import Web.Scotty
import AppContext (initializeApp)
import BitonicController (routes)

main :: IO ()
main = do
    envResult <- initializeApp
    case envResult of
        Left err -> do
            putStrLn err
            putStrLn "Exiting."
        Right env -> do
            putStrLn "Starting Bitonic Sequence API on http://localhost:3000"
            scotty 3000 $ routes env
