{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.List hiding (find)
import Data.List.Split
import Data.String.Utils (endswith, replace)
import Safe
import Data.Char

parseEmbedded :: String -> String
parseEmbedded str =
  case splitOn "#" str of
    [name, typ] ->
      if endswith "@" typ
        then "{\"name\":\"" ++ name ++ "\",\"type\":\"" ++ replace "@" "" typ ++ "\",\"last\":true}"
        else "{\"name\":\"" ++ name ++ "\",\"type\":\"" ++ typ ++ "\"}"
    _ -> ""

processPart :: String -> String
processPart str =
  let symb = headDef ' ' str
  in if isDigit symb
     then str
     else if symb `elem` specialCharacters
          then str
          else if "#" `isInfixOf` str
               then parseEmbedded str
               else "\"" ++ str ++ "\""

simpleParamsToJson :: String -> String
simpleParamsToJson sparams =
  "{" ++  (concatMap processPart $ split (dropBlanks $ oneOf specialCharacters) sparams) ++ "}"

specialCharacters :: [Char]
specialCharacters = ",:[]{}"
