{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.AuthHeader where

import Network.Wai
import Network.Wai.Handler.Warp
import Data.ByteString.Base64 (decodeLenient)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word8 (isSpace, _colon, toLower)
import Data.Hash.MD5

extractBasicAuth :: T.Text -> Maybe (T.Text, T.Text)
extractBasicAuth bs =
  let (x, y) = B.break isSpace $ T8.encodeUtf8 bs
  in if B.map toLower x == "basic"
     then decode $ extract $ B.dropWhile isSpace y
     else Nothing
  where extract encoded =
            let rawstr = decodeLenient encoded
                (username, password') = B.break (== _colon) rawstr
            in ((username,) . snd) <$> B.uncons password'
        decode Nothing = Nothing
        decode (Just (u,p)) = Just (T8.decodeUtf8 u, T8.decodeUtf8 p)

encodePassword :: String -> String -> String
encodePassword salt pwd = md5s $ Str $ salt ++ pwd
