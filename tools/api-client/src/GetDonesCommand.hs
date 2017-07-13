
{-# LANGUAGE OverloadedStrings #-}

module GetDonesCommand where

import qualified Data.ByteString.Char8 as C
import Network.HTTP.Simple
import Data.Aeson
import ApiClientArgs
import AppConfig

getDones :: Options -> IO ()
getDones opts = do
    d <- loadConfig $ optConfig opts
    case d of
        Left  err    -> putStrLn err
        Right config -> getDones_ config

getDones_ :: AppConfig -> IO ()
getDones_ config = do
    sessionId <- readFile $ sessionFile config
    initReq <- parseRequest $ apiEndpoint config ++ "/dones"
    let req = addRequestHeader "Cookie" (C.pack sessionId) initReq
    res <- httpJSON req
    let responseBody = getResponseBody res :: Value
    print responseBody
