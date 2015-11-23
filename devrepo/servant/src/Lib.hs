{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( mainFunc
    ) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Data.Text
import Servant

import Database.Persist.MySQL
import Control.Monad.Logger

import Auth.AuthHeader
import Auth.DbAuth
import AppConfig
import Db.Common


import Control.Monad.IO.Class
import Control.Monad.Trans.Either


configFileName :: String
configFileName = "application.conf"


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost = dbHost conf
                                      , connectPort = dbPort conf
                                      }

---------------------------------------
-- Examples section

helloUser :: ConnectionPool -> String -> Maybe Text -> EitherT ServantErr IO Text
helloUser pool salt Nothing = left $ err403 { errBody = "No Authorization header found!" }
helloUser pool salt (Just authHeader) =
    withUser pool authHeader salt $ \user -> do
        liftIO $ print user
        return $ pack $ show user

hello :: EitherT ServantErr IO Text
hello  = return "Hello, World!"

---------------------------------------

type API = "helloUser" :> Header "Authorization" Text :> Get '[PlainText] Text
      :<|> "hello" :> Get '[PlainText] Text

type API' = API :<|> Raw


api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: ConnectionPool -> String -> Server API
server pool salt = helloUser pool salt
              :<|> hello

server' :: ConnectionPool -> String -> Server API'
server' pool salt = server pool salt
               :<|> serveDirectory "static"


---------------------------------------

mainFunc :: IO ()
mainFunc = do
    putStrLn "Starting server..."
    loadedConf <- C.load [C.Required configFileName]
    maybeConf <- makeDbConfig loadedConf
    case maybeConf of
        Nothing -> putStrLn $ "Can't parse \"" ++ configFileName ++ "\" file, terminating!"
        Just conf -> do
            pool <- runStdoutLoggingT $ createMySQLPool (connectInfo conf) (fromIntegral $ poolSize conf)
            run (fromIntegral $ appPort conf) (serve api' (server' pool (dbSalt conf)))
