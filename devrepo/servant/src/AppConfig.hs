{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AppConfig where

import GHC.Generics (Generic)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Word

data AppConfig = AppConfig {
       dbName     :: String
     , dbUser     :: String
     , dbPassword :: String
     , dbSalt     :: String
     , dbHost     :: String
     , dbPort     :: Word16
     , appPort    :: Word16
     , poolSize   :: Word16
     }
     deriving (Show, Generic)

makeDbConfig :: C.Config -> IO (Maybe AppConfig)
makeDbConfig conf = do
  name     <- C.lookup conf "database.name"         :: IO (Maybe String)
  user     <- C.lookup conf "database.user"         :: IO (Maybe String)
  password <- C.lookup conf "database.password"     :: IO (Maybe String)
  dsalt    <- C.lookup conf "database.salt"         :: IO (Maybe String)
  host     <- C.lookup conf "database.host"         :: IO (Maybe String)
  dport    <- C.lookup conf "database.port"         :: IO (Maybe Word16)
  port     <- C.lookup conf "application.port"      :: IO (Maybe Word16)
  psize    <- C.lookup conf "application.poolSize"  :: IO (Maybe Word16)
  return $ AppConfig <$> name
                     <*> user
                     <*> password
                     <*> dsalt
                     <*> host
                     <*> dport
                     <*> port
                     <*> psize
