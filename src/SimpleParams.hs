{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.List hiding (find)
import Data.String.Utils
import Safe

data Parsed =  ParsedObject String String
             | ParsedList [Parsed] String
     deriving (Eq, Ord, Read, Show)

extractPortion :: String -> String -> Parsed
extractPortion extracted restStr = 
  let symb = headDef ',' restStr
      continueExtracting = 
        if (length restStr) >= 1 then extractPortion (extracted ++ [symb]) (tail restStr)
        else ParsedObject extracted restStr
  in case symb of
       ',' -> ParsedObject extracted (tailDef "" restStr)
       '}' -> ParsedObject extracted (tailDef "" restStr)
       ']' -> ParsedObject extracted (tailDef "" restStr)
       '{' -> ParsedObject extracted (tailDef "" restStr) -- processString
       '[' -> ParsedObject extracted (tailDef "" restStr) -- processString
       _  -> continueExtracting

makePropertyTuple :: String -> String
makePropertyTuple stuple =
  let splitted = split ":" stuple
      prepareTuple [] = ""
      prepareTuple [key] = "\"" ++ key ++ "\":\"\""
      prepareTuple [key, value] = "\"" ++ key ++ "\":\"" ++ value ++ "\""
      prepareTuple [key, value, _] = "\"" ++ key ++ "\":\"" ++ value ++ "\""
      prepareTuple _ = ""
  in prepareTuple splitted

makeObjectTuple :: String -> String
makeObjectTuple stuple =
  let splitted = split "#" stuple
      prepareTuple [] = "{}"
      prepareTuple [key] = "{\"name\":" ++ key ++ "\",\"type\":\"\"}"
      prepareTuple [key, value] = "{\"name\":" ++ key ++ "\",\"type\":\"" ++ value ++ "\"}"
      prepareTuple [key, value, _] = "{\"name\":" ++ key ++ "\",\"type\":\"" ++ value ++ "\"}"
      prepareTuple _ = ""
  in prepareTuple splitted


processString :: Parsed -> Parsed
processString (ParsedObject result "") = ParsedObject (tailDef "" result) ""
processString (ParsedObject processed restStr) =
  let (ParsedObject extracted newRestStr) = extractPortion "" restStr
      tuple = if "#" `isInfixOf` extracted then makeObjectTuple extracted
              else makePropertyTuple extracted
  in processString $ ParsedObject (processed ++ "," ++ tuple) newRestStr


simpleParamsToJson :: String -> String
simpleParamsToJson sparams = 
  let s = replace " " "" sparams
      ParsedObject processed _ = processString $ ParsedObject "" s
  in "{" ++ processed ++ "}"
