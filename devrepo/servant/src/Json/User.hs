{-# LANGUAGE DeriveGeneric #-}

module Json.User where

import GHC.Generics (Generic)
import qualified Data.Aeson as A

data User = User {
    id       :: Maybe Int
  , login    :: String
  , password :: String
  }
  deriving (Show, Generic)

instance A.ToJSON User
instance A.FromJSON User
