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
import GHC.Generics
import Network.HTTP.Conduit
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as B

data Options = Options  { optConfig     :: String
                        , optAction     :: String
                        }

startOptions :: Options
startOptions = Options  { optConfig     = ""
                        , optAction     = ""
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "c" ["config"]
        (ReqArg
            (\arg opt -> return opt { optConfig = arg })
            "FILE")
        "Config file"

    , Option "a" ["action"]
        (ReqArg
            (\arg opt -> return opt { optAction = arg })
            "ACTION_NAME")
        "Action name. Currently only \"login\""

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs

    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    opts <- Prelude.foldl (>>=) (return startOptions) actions

    let Options { optConfig = config
                , optAction = action   } = opts

    d <- (eitherDecode <$> B.readFile config) :: IO (Either String LoginInfo)

    manager <- newManager tlsManagerSettings
    req <- parseRequest "https://whatsdone-api.ryuichi.io/signin"

    putStrLn $ "Action: " ++ action
    case d of
        Left err -> putStrLn err
        Right ps -> runResourceT $ do
            let reqHead = urlEncodedBody [("email", encodeUtf8 $ email ps), ("password", encodeUtf8 $ password ps)] req
            res <- http reqHead manager
            liftIO $ do
                print $ responseStatus res
                mapM_ print $ responseHeaders res

data LoginInfo = LoginInfo { email :: Text
                           , password :: Text
                           } deriving (Show, Generic)

instance FromJSON LoginInfo
instance ToJSON LoginInfo
