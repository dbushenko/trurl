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
processPart str
    | isDigit symb                  = str
    | symb `elem` specialCharacters = str
    | '#'  `elem` str               = parseEmbedded str
    | otherwise                     = "\"" ++ str ++ "\""
  where
    symb = headDef ' ' str

simpleParamsToJson :: String -> String
simpleParamsToJson sparams =
    "{" ++  concatMap processPart (splitOnSpecialCharacters sparams) ++ "}"

splitOnSpecialCharacters :: String -> [String]
splitOnSpecialCharacters = split $ dropBlanks $ oneOf specialCharacters

specialCharacters :: [Char]
specialCharacters = ",:[]{}"
