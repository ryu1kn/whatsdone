
{-# LANGUAGE OverloadedStrings #-}

module GetDonesCommand where

import           ApiClientArgs
import           AppConfig
import           Data.Aeson
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as S8
import           Network.HTTP.Simple

getDones :: Options -> IO ()
getDones opts = do
    d <- loadConfig $ optConfig opts
    case d of
        Left  err    -> putStrLn err
        Right config -> getDones_ config

getDones_ :: AppConfig -> IO ()
getDones_ config = do
    sessionId <- readFile $ sessionFile config
    initReq   <- parseRequest $ apiEndpoint config ++ "/dones"
    let req = addRequestHeader "Cookie" (C.pack sessionId) initReq
    res <- httpJSON req
    S8.putStrLn $ encode (getResponseBody res :: Value)
