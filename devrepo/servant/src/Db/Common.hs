{-# LANGUAGE FlexibleContexts #-}
module Db.Common where

import Control.Monad.IO.Class
import Database.Persist.MySQL

query :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
query pool query = liftIO $ runSqlPool query pool

toKey :: ToBackendKey SqlBackend record => Int -> Key record
toKey _id = toSqlKey $ fromIntegral _id

