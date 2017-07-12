{-# LANGUAGE DeriveGeneric #-}

module AppConfig where

import Data.Aeson
import Data.Text
import GHC.Generics

data AppConfig = AppConfig { email :: Text
                           , password :: Text
                           } deriving (Show, Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig
