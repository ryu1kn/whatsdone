
module Main (main) where

import           ApiClientArgs
import           GetDonesCommand    (getDones)
import           LoginCommand       (login)
import           System.Environment

main :: IO ()
main = do
    (action, opts) <- getArgs >>= parse

    case action of
        "login"     -> login opts
        "get-dones" -> getDones opts
        _           -> putStrLn "Only \"login\" action is currently supported"
