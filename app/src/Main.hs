-- Main module - application entry point (like public static void main in Java)
module Main where

-- Import Scotty web framework
-- scotty is the main function to start the server
import Web.Scotty

-- Import application initialization function
-- initializeApp sets up Redis connection and environment
import AppContext (initializeApp)

-- Import HTTP route definitions
-- routes defines all API endpoints (POST /bitonic, etc.)
import BitonicController (routes)

-- Main function - entry point of the application
-- Type signature: main is an IO action that returns ()
-- () is "unit" - like void in Java
main :: IO ()

-- Function implementation
-- "do" starts a block of sequential IO actions
main = do
    -- Initialize application environment (connects to Redis)
    -- Returns Either String AppEnv:
    --   Left String = error message if failed
    --   Right AppEnv = environment with Redis connection if successful
    -- <- binds the result to envResult
    envResult <- initializeApp
    
    -- Pattern match on Either (like switch/case in Java)
    case envResult of
        -- If initialization failed (Left contains error message)
        Left err -> do
            -- Print error message to console
            putStrLn err
            putStrLn "Exiting."
            -- Program ends here if Redis connection fails
        
        -- If initialization succeeded (Right contains the environment)
        -- env now contains our AppEnv with Redis connection
        Right env -> do
            putStrLn "Starting Bitonic Sequence API on http://localhost:3000"
            
            -- Start Scotty web server on port 3000
            -- $ is function application (avoids parentheses)
            -- routes env defines all HTTP endpoints with access to environment
            scotty 3000 $ routes env
