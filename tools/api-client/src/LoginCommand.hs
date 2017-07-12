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
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import ApiClientArgs
import AppConfig

apiEndpoint = "https://whatsdone-api.ryuichi.io"
sessionFile = "__session.txt"

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
                let Just (_, cookie) = L.find (\(x, y) -> x == "Set-Cookie") $ responseHeaders res
                B.writeFile sessionFile cookie
                putStrLn $ "Login successful, session id stored in " ++ sessionFile

loadConfig :: FilePath -> IO (Either String AppConfig)
loadConfig path = eitherDecode <$> BL.readFile path

requestLogin (email, password, manager) = do
    req <- parseRequest $ apiEndpoint ++ "/signin"
    let reqHead = urlEncodedBody [("email", encodeUtf8 email), ("password", encodeUtf8 password)] req
    http reqHead manager
