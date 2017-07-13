
{-# LANGUAGE OverloadedStrings #-}

module GetDonesCommand where

import Data.Text
import Network.HTTP.Simple
import ApiClientArgs
import AppConfig

sessionFile = "__session.txt"

getDones :: Options -> IO ()
getDones opts = do
    d <- loadConfig $ optConfig opts
    case d of
        Left  err    -> putStrLn err
        Right config -> getDones_ config

getDones_ :: AppConfig -> IO ()
getDones_ config = do
    requestDones config "connect.sid=s%3A3fc5d5c8-6f50-4da3-b361-854456cad538.W3xzaA6XPAgLhuPPxKy97tE1c0BQBkLvZLTRfRYfVyo"

requestDones config sessionId = do
    initReq <- parseRequest $ apiEndpoint config ++ "/dones"
    let req = addRequestHeader "Cookie" sessionId initReq
    res <- httpJSON req
    putStrLn $ getResponseBody res
