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

data {{Name}} = {{Name}} Integer{{#props}} {{type}}{{/props}}
     deriving (Show)

instance FromJSON {{Name}} where
     parseJSON (Object v) = {{Name}}
                            <$> v .:? "id" .!= 0  -- the field "id" is optional
                            {{#props}}<*> v .: "{{name}}"
                            {{/props}}


instance ToJSON {{Name}} where
     toJSON ({{Name}} aid {{#props}}{{name}} {{/props}}) =
         object [{{#props}}"{{name}}" .= {{name}},
                 {{/props}}
"id" .= aid]                 


-- get   "/{{Name}}s" $ do entities <- liftIO $ list{{Name}}s pool
--                         Web.Scotty.json entities

list{{Name}}s :: Pool Connection -> IO [{{Name}}]
list{{Name}}s pool = do
     res <- fetchSimple pool "SELECT * FROM {{Name}} ORDER BY id DESC" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ fmap (\(aid{{#props}}, {{name}}{{/props}}) -> {{Name}} aid{{#props}} {{name}}{{/props}}) res

-- get   "/{{Name}}s/:id" $ do aid <- param "id" :: ActionM TL.Text
--                             maybeEntity <- liftIO $ find{{Name}} pool aid
--                             viewEntity maybeEntity
--                             where viewEntity Nothing = Web.Scotty.json ()
--                                   viewEntity (Just e) = Web.Scotty.json e
   
find{{Name}} :: Pool Connection -> Text -> IO (Maybe {{Name}})
find{{Name}} pool aid = do
     res <- fetch pool (Only aid) "SELECT * FROM {{Name}} WHERE id=?" :: IO [(Integer{{#props}}, {{type}}{{/props}})]
     return $ one{{Name}} res
     where one{{Name}} ((oaid{{#props}}, {{name}}{{/props}}) : _) = Just $ {{Name}} oaid{{#props}} {{name}}{{/props}}
           one{{Name}} _ = Nothing


-- post  "/{{Name}}s" $ do entity <- getEntityParam
--                         insert{{Name}} pool entity
--                         Web.Scotty.json ()

insert{{Name}} :: Pool Connection -> Maybe {{Name}} -> ActionT Text IO ()
insert{{Name}} _ Nothing = return ()
insert{{Name}} pool (Just ({{Name}} _ {{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [{{#props}}(show {{name}}){{^last}}, {{/last}}{{/props}}] "INSERT INTO {{Name}}({{#props}}{{name}}{{^last}}, {{/last}}{{/props}}) VALUES({{#props}}?{{^last}},{{/last}}{{/props}})"
     return ()

-- put   "/{{Name}}s" $ do entity <- getEntityParam
--                         update{{Name}} pool entity
--                         Web.Scotty.json ()

update{{Name}} :: Pool Connection -> Maybe {{Name}} -> ActionT Text IO ()
update{{Name}} _ Nothing = return ()
update{{Name}} pool (Just ({{Name}} aid{{#props}} {{name}}{{/props}})) = do
     liftIO $ execSqlT pool [{{#props}}(show {{name}}), {{/props}}(show $ decodeUtf8 $ BL.pack $ show aid)]
                            "UPDATE {{Name}} SET {{#props}}{{name}}=?{{^last}},{{/last}} {{/props}}WHERE id=?"
     return ()


-- delete "/{{Name}}s/:id" $ do aid <- param "id" :: ActionM TL.Text
--                              delete{{Name}} pool aid
--                              Web.Scotty.json ()

delete{{Name}} :: Pool Connection -> Text -> ActionT Text IO ()
delete{{Name}} pool aid = do
     liftIO $ execSqlT pool [aid] "DELETE FROM {{Name}} WHERE id=?"
     return ()
