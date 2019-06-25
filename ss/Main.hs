{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.GRPC.HighLevel.Generated
import System.Environment

main :: IO ()
main = do
    putStrLn "Starting test"
    setEnv "GRPC_TRACE" "tcp,http,api"
    setEnv "GRPC_VERBOSITY" "debug"
    let clientConfig = ClientConfig (Host "localhost") (Port 10000) [] Nothing
    withGRPCClient clientConfig $ \_client ->
        putStrLn "OK"
    putStrLn "Test done"
