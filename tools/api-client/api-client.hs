module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad

data Options = Options  { optConfig     :: IO String
                        , optAction     :: String
                        }

startOptions :: Options
startOptions = Options  { optConfig     = getContents
                        , optAction     = ""
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "c" ["config"]
        (ReqArg
            (\arg opt -> return opt { optConfig = readFile arg })
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

    opts <- foldl (>>=) (return startOptions) actions

    let Options { optConfig = input
                , optAction = action   } = opts

    putStrLn $ "Action: " ++ action
