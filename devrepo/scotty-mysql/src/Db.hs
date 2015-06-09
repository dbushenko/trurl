{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where

import GHC.Generics (Generic)
import qualified Database.MySQL.Base as M
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Data.Pool(Pool, withResource)
import GHC.Int

-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     }
     deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (QueryResults r, QueryParams q) => Pool M.Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: QueryResults r => Pool M.Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

-- Update database
execSql :: QueryParams q => Pool M.Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
fetchT :: (QueryResults r, QueryParams q) => Pool M.Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
      where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: QueryResults r => Pool M.Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
       where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: QueryParams q => Pool M.Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------
