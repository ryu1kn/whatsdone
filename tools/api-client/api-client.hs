#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-8.21 runghc
    --package http-conduit
    --package yaml
    --package tls-1.3.11
    --package cryptonite-0.21
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString.Lazy as B
import ApiClientArgs
import LoginInfo

apiEndpoint = "https://whatsdone-api.ryuichi.io/signin"

main :: IO ()
main = do
    (action, opts) <- getArgs >>= parse

    d <- (eitherDecode <$> B.readFile (optConfig opts :: FilePath)) :: IO (Either String LoginInfo)

    manager <- newManager tlsManagerSettings
    req <- parseRequest apiEndpoint

    putStrLn $ "Action: " ++ action
    case d of
        Left err -> putStrLn err
        Right ps -> runResourceT $ do
            let reqHead = urlEncodedBody [("email", encodeUtf8 $ email ps), ("password", encodeUtf8 $ password ps)] req
            res <- http reqHead manager
            liftIO $ do
                print $ responseStatus res
                mapM_ print $ responseHeaders res
