{-# LANGUAGE OverloadedStrings #-}

module Article where

import Db

import Data.Text.Lazy
import Data.Aeson
import Control.Applicative
import Web.Scotty.Internal.Types (ActionT)
import Control.Monad.IO.Class
import Database.MySQL.Simple
import Data.Pool(Pool)
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy.Char8 as BL

data Article = Article Integer Text Text -- id title bodyText
     deriving (Show)

instance FromJSON Article where
     parseJSON (Object v) = Article <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "title"    <*>
                            v .:  "bodyText"


instance ToJSON Article where
     toJSON (Article aid title bodyText) =
         object ["id" .= aid,
                 "title" .= title,
                 "bodyText" .= bodyText]
                 


listArticles :: Pool Connection -> IO [Article]
listArticles pool = do
     res <- fetchSimple pool "SELECT * FROM article ORDER BY id DESC" :: IO [(Integer, Text, Text)]
     return $ fmap (\(aid, title, bodyText) -> Article aid title bodyText) res
   
findArticle :: Pool Connection -> Text -> IO (Maybe Article)
findArticle pool aid = do
     res <- fetch pool (Only aid) "SELECT * FROM article WHERE id=?" :: IO [(Integer, Text, Text)]
     return $ oneArticle res
     where oneArticle ((oaid, title, bodyText) : _) = Just $ Article oaid title bodyText
           oneArticle _ = Nothing


insertArticle :: Pool Connection -> Maybe Article -> ActionT Text IO ()
insertArticle _ Nothing = return ()
insertArticle pool (Just (Article _ title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText] "INSERT INTO article(title, bodyText) VALUES(?,?)"
     return ()

updateArticle :: Pool Connection -> Maybe Article -> ActionT Text IO ()
updateArticle _ Nothing = return ()
updateArticle pool (Just (Article aid title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText, (decodeUtf8 $ BL.pack $ show aid)]
                            "UPDATE article SET title=?, bodyText=? WHERE id=?"
     return ()

deleteArticle :: Pool Connection -> Text -> ActionT Text IO ()
deleteArticle pool aid = do
     liftIO $ execSqlT pool [aid] "DELETE FROM article WHERE id=?"
     return ()
