module Convert.UserConverter where

import Prelude hiding (id)
import GHC.Generics (Generic)
import qualified Data.Aeson as A

import Database.Persist.MySQL
import Data.Text

import Db.Common
import qualified Db.User as D
import Json.User


toEntity (User maybeId _login _password) =
    case maybeId of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key D.User) $
                                D.User _login _password

toRecord (User _ _login _password) = D.User _login _password

toJson (Entity key (D.User _login _password)) =
    let vals = keyToValues key
    in processKeys vals
    
    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ User (Just $ fromIntegral _id) _login _password
                
          processKeys _ = Nothing

