#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-8.21 runghc
    --package http-conduit
    --package yaml
    --package tls-1.3.11
    --package cryptonite-0.21
-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.List as L
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import ApiClientArgs
import AppConfig

apiEndpoint = "https://whatsdone-api.ryuichi.io/signin"

main :: IO ()
main = do
    (action, opts) <- getArgs >>= parse

    if action == "login"
        then login opts
        else putStrLn "Only \"login\" action is currently supported"

loadConfig :: FilePath -> IO (Either String AppConfig)
loadConfig path = eitherDecode <$> BL.readFile path

login :: Options -> IO ()
login opts = do
    manager <- newManager tlsManagerSettings
    d <- loadConfig $ optConfig opts
    case d of
        Left err -> putStrLn err
        Right config -> runResourceT $ do
            res <- requestLogin ( email config
                                , password config
                                , manager
                                )
            liftIO $ do
                print $ responseStatus res
                let Just (_, cookie) = L.find (\(x, y) -> x == "Set-Cookie") $ responseHeaders res
                B.writeFile "session.txt" cookie

requestLogin (email, password, manager) = do
    req <- parseRequest apiEndpoint
    let reqHead = urlEncodedBody [("email", encodeUtf8 email), ("password", encodeUtf8 password)] req
    http reqHead manager
