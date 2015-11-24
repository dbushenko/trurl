{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.List.Split
import Data.String.Utils (endswith, replace)
import Safe
import Data.Char

parseEmbedded :: String -> String
parseEmbedded str =
  case splitOn "#" str of
    [k, v] ->
      if endswith "@" v
        then "{\"key\":\"" ++ k ++ "\",\"value\":\"" ++ replace "@" "" v ++ "\",\"last\":true}"
        else "{\"key\":\"" ++ k ++ "\",\"value\":\"" ++ v ++ "\"}"
    _ -> ""

processPart :: String -> String
processPart str
    | isDigit symb                  = str
    | symb `elem` specialCharacters = str
    | '#'  `elem` str               = parseEmbedded str
    | otherwise                     = "\"" ++ str ++ "\""
  where
    symb = headDef ' ' str

simpleParamsToJson :: String -> String
simpleParamsToJson sparams = "{" ++  concatMap processPart (splitOnSpecialCharacters sparams) ++ "}"

splitOnSpecialCharacters :: String -> [String]
splitOnSpecialCharacters = split $ dropBlanks $ oneOf specialCharacters

specialCharacters :: String
specialCharacters = ",:[]{} "
