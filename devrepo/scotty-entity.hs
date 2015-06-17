{-# LANGUAGE OverloadedStrings #-}

module {{FileName}} where

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

data {{FileName}} = {{FileName}} Integer{{#props}} {{type}}{{/props}}
     deriving (Show)

instance FromJSON {{FileName}} where
     parseJSON (Object v) = {{FileName}}
                            <$> v .:? "id" .!= 0  -- the field "id" is optional
                            {{#props}}<*> v .: "{{name}}"
                            {{/props}}


instance ToJSON {{FileName}} where
     toJSON ({{FileName}} aid {{#props}}{{name}} {{/props}}) =
         object [{{#props}}"{{name}}" .= {{name}},
                 {{/props}}
"id" .= aid]                 


-- get   "/{{FileName}}s" $ do entities <- liftIO $ list{{FileName}}s pool
--                         Web.Scotty.json entities

list{{FileName}}s :: Pool Connection -> IO [{{FileName}}]
list{{FileName}}s pool = do
     res <- fetchSimple pool "SELECT * FROM {{FileName}} ORDER BY id DESC" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ fmap (\(aid{{#props}}, {{name}}{{/props}}) -> {{FileName}} aid{{#props}} {{name}}{{/props}}) res

-- get   "/{{FileName}}s/:id" $ do aid <- param "id" :: ActionM TL.Text
--                             maybeEntity <- liftIO $ find{{FileName}} pool aid
--                             viewEntity maybeEntity
--                             where viewEntity Nothing = Web.Scotty.json ()
--                                   viewEntity (Just e) = Web.Scotty.json e
   
find{{FileName}} :: Pool Connection -> Text -> IO (Maybe {{FileName}})
find{{FileName}} pool aid = do
     res <- fetch pool (Only aid) "SELECT * FROM {{FileName}} WHERE id=?" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ one{{FileName}} res
     where one{{FileName}} ((oaid{{#props}}, {{name}}{{/props}}) : _) = Just $ {{FileName}} oaid{{#props}} {{name}}{{/props}}
           one{{FileName}} _ = Nothing


-- post  "/{{FileName}}s" $ do entity <- getEntityParam
--                         insert{{FileName}} pool entity
--                         Web.Scotty.json ()

insert{{FileName}} :: Pool Connection -> Maybe {{FileName}} -> ActionT Text IO ()
insert{{FileName}} _ Nothing = return ()
insert{{FileName}} pool (Just ({{FileName}} _ {{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [{{#props}}(show {{name}}){{^last}}, {{/last}}{{/props}}] "INSERT INTO {{FileName}}({{#props}}{{name}}{{^last}}, {{/last}}{{/props}}) VALUES({{#props}}?{{^last}},{{/last}}{{/props}})"
     return ()

-- put   "/{{FileName}}s" $ do entity <- getEntityParam
--                         update{{FileName}} pool entity
--                         Web.Scotty.json ()

update{{FileName}} :: Pool Connection -> Maybe {{FileName}} -> ActionT Text IO ()
update{{FileName}} _ Nothing = return ()
update{{FileName}} pool (Just ({{FileName}} aid{{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [{{#props}}(show {{name}}), {{/props}}(show $ decodeUtf8 $ BL.pack $ show aid)]
                            "UPDATE {{FileName}} SET {{#props}}{{name}}=?{{^last}},{{/last}} {{/props}}WHERE id=?"
     return ()


-- delete "/{{FileName}}s/:id" $ do aid <- param "id" :: ActionM TL.Text
--                              delete{{FileName}} pool aid
--                              Web.Scotty.json ()

delete{{FileName}} :: Pool Connection -> Text -> ActionT Text IO ()
delete{{FileName}} pool aid = do
     liftIO $ execSqlT pool [aid] "DELETE FROM {{FileName}} WHERE id=?"
     return ()
