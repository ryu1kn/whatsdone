
{-# LANGUAGE OverloadedStrings #-}

module GetDonesCommand where

import Network.HTTP.Simple
import ApiClientArgs

apiEndpoint = "https://whatsdone-api.ryuichi.io"
sessionFile = "__session.txt"

getDones :: Options -> IO ()
getDones opts = do
    requestDones "connect.sid=s%3A3fc5d5c8-6f50-4da3-b361-854456cad538.W3xzaA6XPAgLhuPPxKy97tE1c0BQBkLvZLTRfRYfVyo"

requestDones sessionId = do
    initReq <- parseRequest $ apiEndpoint ++ "/dones"
    let req = addRequestHeader "Cookie" sessionId initReq
    res <- httpJSON req
    putStrLn $ getResponseBody res
