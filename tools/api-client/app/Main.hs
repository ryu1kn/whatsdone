
module Main (main) where

import System.Console.GetOpt
import System.Environment
import LoginCommand (login)
import GetDonesCommand (getDones)
import ApiClientArgs

main :: IO ()
main = do
    (action, opts) <- getArgs >>= parse

    case action of
        "login"     -> login opts
        "get-dones" -> getDones opts
        _           -> putStrLn "Only \"login\" action is currently supported"
