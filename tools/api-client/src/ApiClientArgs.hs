
module ApiClientArgs (parse, Options(..)) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Options = Options  { optConfig     :: String
                        }

startOptions :: Options
startOptions = Options  { optConfig     = ""
                        }

optionDefinitions :: [ OptDescr (Options -> IO Options) ]
optionDefinitions =
    [ Option "c" ["config"]
        (ReqArg
            (\arg opt -> return opt { optConfig = arg })
            "FILE")
        "Config file"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg optionDefinitions)
                exitSuccess))
        "Show help"
    ]

parse :: [String] -> IO (String, Options)
parse args = do
    let (options, nonOptions, errors) = getOpt Permute optionDefinitions args
    let action = head nonOptions
    opts <- Prelude.foldl (>>=) (return startOptions) options
    return (action, opts)
