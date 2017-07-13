{-# LANGUAGE DeriveGeneric #-}

module AppConfig where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

data AppConfig = AppConfig { email :: String
                           , password :: String
                           , apiEndpoint :: String
                           } deriving (Show, Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig

loadConfig :: FilePath -> IO (Either String AppConfig)
loadConfig path = eitherDecode <$> BL.readFile path
