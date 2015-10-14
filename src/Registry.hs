{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry where

import Data.Aeson
import GHC.Generics (Generic)

data Registry = Registry { url :: String
                         , templateName :: String
                         , metainfoName :: String
                         }
  deriving (Show, Generic)                

instance ToJSON Registry
instance FromJSON Registry
