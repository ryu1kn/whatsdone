{-# LANGUAGE OverloadedStrings #-}

module LoginCommand where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.List as L
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import ApiClientArgs
import AppConfig

sessionFile = "__session.txt"

login :: Options -> IO ()
login opts = do
    manager <- newManager tlsManagerSettings
    d <- loadConfig $ optConfig opts
    case d of
        Left  err    -> putStrLn err
        Right config -> login_ config manager

login_ :: AppConfig -> Manager -> IO ()
login_ config manager = runResourceT $ do
    res <- requestLogin config manager
    liftIO $ do
        let maybeCookie = L.find (\(x, y) -> x == "Set-Cookie") $ responseHeaders res
        case maybeCookie of
            Nothing -> return ()
            Just (_, cookie) -> do
                B.writeFile sessionFile cookie
                putStrLn $ "Login successful, session id stored in " ++ sessionFile

requestLogin config manager = do
    req <- parseRequest $ unpack (apiEndpoint config) ++ "/signin"
    let reqHead = urlEncodedBody [ ("email", encodeUtf8 $ email config)
                                 , ("password", encodeUtf8 $ password config) ] req
    http reqHead manager
