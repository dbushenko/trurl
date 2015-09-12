{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.List hiding (find)
import Data.String.Utils
import Safe
import Data.Char

parseEmbedded :: String -> String
parseEmbedded str =
  case split "#" str of
    [name, typ] ->
      if endswith "@" typ
        then "{\"name\":\"" ++ name ++ "\",\"type\":\"" ++ replace "@" "" typ ++ "\",\"last\":true}"
        else "{\"name\":\"" ++ name ++ "\",\"type\":\"" ++ typ ++ "\"}"
    _ -> ""

processPart :: String -> String
processPart str =
  let symb = headDef ' ' str
  in if isDigit symb then str
     else if symb == ','
           || symb == '['
           || symb == '{'
           || symb == ']'
           || symb == '}'
           || symb == ':'
             then str
          else if "#" `isInfixOf` str then parseEmbedded str
               else "\"" ++ str ++ "\""

simpleParamsToJson :: String -> String
simpleParamsToJson sparams =
  let s =  (replace "," " , ")
         . (replace "[" " [ ")
         . (replace "{" " { ")
         . (replace "]" " ] ")
         . (replace "}" " } ")
         . (replace ":" " : ")
         $ sparams
  in "{" ++  (concatMap processPart $ splitWs s) ++ "}"
