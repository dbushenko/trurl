{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import Article
import Web.Scotty
import qualified Data.Text.Lazy as TL

articlesList :: [Article] -> ActionM ()
articlesList articles = json articles

viewArticle :: Maybe Article -> ActionM ()
viewArticle Nothing = json ()
viewArticle (Just article) = json article

createdArticle :: Maybe Article -> ActionM ()
createdArticle _ = json ()

updatedArticle :: Maybe Article -> ActionM ()
updatedArticle _ = json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle _ = json ()
