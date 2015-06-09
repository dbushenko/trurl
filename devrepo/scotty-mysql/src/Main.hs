{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db
import Views
import Article

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.MySQL.Simple
import Data.Pool(createPool)
import qualified Data.Text.Lazy as TL
import Data.Aeson

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    
    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do      
          pool <- createPool (newConn conf) close 1 64 10
          scotty 3000 $ do
              middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
              middleware logStdout                                    -- log all requests; for production use logStdout

              -- LIST
              get   "/articles" $ do articles <- liftIO $ listArticles pool  -- get the ist of articles for DB
                                     articlesList articles                   -- show articles list

              -- VIEW
              get   "/articles/:id" $ do aid <- param "id" :: ActionM TL.Text -- get the article id from the request
                                         maybeArticle <- liftIO $ findArticle pool aid -- get the article from the DB
                                         viewArticle maybeArticle            -- show the article if it was found

              -- CREATE
              post  "/admin/articles" $ do article <- getArticleParam -- read the request body, try to parse it into article
                                           insertArticle pool article -- insert the parsed article into the DB
                                           createdArticle article     -- show info that the article was created

              -- UPDATE
              put   "/admin/articles" $ do article <- getArticleParam -- read the request body, try to parse it into article
                                           updateArticle pool article -- update parsed article in the DB
                                           updatedArticle article     -- show info that the article was updated

              -- DELETE
              delete "/admin/articles/:id" $ do aid <- param "id" :: ActionM TL.Text -- get the article id
                                                deleteArticle pool aid  -- delete the article from the DB
                                                deletedArticle aid      -- show info that the article was deleted

-----------------------------------------------

-- Parse the request body into the JSON object
getArticleParam :: FromJSON t => ActionT TL.Text IO (Maybe t)
getArticleParam = do b <- body
                     return (decode b :: FromJSON t => Maybe t)
