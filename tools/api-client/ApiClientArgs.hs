
module ApiClientArgs (parse, Options(..)) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

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

parse :: [String] -> IO Options
parse commandArgs = do
    let (actions, nonOptions, errors) = getOpt RequireOrder options commandArgs
    Prelude.foldl (>>=) (return startOptions) actions
