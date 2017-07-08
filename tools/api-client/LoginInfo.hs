{-# LANGUAGE DeriveGeneric #-}

module LoginInfo where

import Data.Aeson
import Data.Text
import GHC.Generics

data LoginInfo = LoginInfo { email :: Text
                           , password :: Text
                           } deriving (Show, Generic)

instance FromJSON LoginInfo
instance ToJSON LoginInfo
