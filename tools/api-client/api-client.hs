#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-5.13 runghc
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import GHC.Generics
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

main = do
    args <- getArgs

    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    opts <- Prelude.foldl (>>=) (return startOptions) actions

    let Options { optConfig = config
                , optAction = action   } = opts

    d <- (eitherDecode <$> B.readFile config) :: IO (Either String LoginInfo)

    putStrLn $ "Action: " ++ action
    case d of
        Left err -> putStrLn err
        Right ps -> print ps


data LoginInfo = LoginInfo { email :: Text
                           , password :: Text
                           } deriving (Show, Generic)

instance FromJSON LoginInfo
instance ToJSON LoginInfo
