{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Db.User where

import GHC.Generics
import Database.Persist.TH
import Database.Persist
import Database.Persist.Sql

share [mkPersist sqlSettings] [persistLowerCase|
User
    login    String
    password String
    deriving Show
    deriving Generic
|]
