{-# LANGUAGE DeriveGeneric #-}

module AppConfig where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

data AppConfig = AppConfig { email :: Text
                           , password :: Text
                           , apiEndpoint :: Text
                           } deriving (Show, Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig

loadConfig :: FilePath -> IO (Either String AppConfig)
loadConfig path = eitherDecode <$> BL.readFile path
