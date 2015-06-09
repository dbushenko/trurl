{-# LANGUAGE OverloadedStrings #-}

module {{Name}} where

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

data {{Name}} = {{Name}} Integer Text Text
     deriving (Show)

instance FromJSON {{Name}} where
     parseJSON (Object v) = {{Name}}
                            <$> v .:? "id" .!= 0  -- the field "id" is optional
                            {{#props}}<*> v .: "{{name}}"
                            {{/props}}


instance ToJSON {{Name}} where
     toJSON ({{Name}} aid {{#props}}{{name}} {{/props}}) =
         object [{{#props}}"{{name}}" .= {{name}}
                 {{/props}}
"id" .= aid]                 


list{{Name}}s :: Pool Connection -> IO [{{Name}}]
list{{Name}}s pool = do
     res <- fetchSimple pool "SELECT * FROM {{Name}} ORDER BY id DESC" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ fmap (\(aid{{#props}}, {{name}}{{/props}}) -> {{Name}} aid{{#props}}, {{name}}{{/props}}) res
   
find{{Name}} :: Pool Connection -> Text -> IO (Maybe {{Name}})
find{{Name}} pool aid = do
     res <- fetch pool (Only aid) "SELECT * FROM {{Name}} WHERE id=?" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ one{{Name}} res
     where one{{Name}} ((oaid{{#props}}, {{name}}{{/props}}) : _) = Just $ {{Name}} oaid{{#props}} {{name}}{{/props}}
           one{{Name}} _ = Nothing


insert{{Name}} :: Pool Connection -> Maybe {{Name}} -> ActionT Text IO ()
insert{{Name}} _ Nothing = return ()
insert{{Name}} pool (Just ({{Name}} _ {{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [title, bodyText] "INSERT INTO {{Name}}({{#props}}{{name}}{{^last}}, {{/last}}{{/props}}) VALUES({{#props}}?{{^last}},{{/last}}{{/props}})"
     return ()

update{{Name}} :: Pool Connection -> Maybe {{Name}} -> ActionT Text IO ()
update{{Name}} _ Nothing = return ()
update{{Name}} pool (Just ({{Name}} aid{{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [{{#props}}{{name}}, {{/props}}(decodeUtf8 $ BL.pack $ show aid)]
                            "UPDATE {{Name}} SET {{#props}}{{name}}=?{{^last}},{{/last}} {{/props}}WHERE id=?"
     return ()

delete{{Name}} :: Pool Connection -> Text -> ActionT Text IO ()
delete{{Name}} pool aid = do
     liftIO $ execSqlT pool [aid] "DELETE FROM {{Name}} WHERE id=?"
     return ()
