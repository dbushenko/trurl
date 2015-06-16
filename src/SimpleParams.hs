{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.String.Utils
import Safe

extractPortion :: String -> String -> (String, String)
extractPortion extracted restStr = 
  let symb = headDef ',' restStr
      continueExtracting = 
        if (length restStr) >= 1 then extractPortion (extracted ++ [symb]) (tail restStr)
        else (extracted, restStr)
  in case symb of
       ',' -> (extracted, (tailDef "" restStr))
       '}' -> (extracted, (tailDef "" restStr))
       ']' -> (extracted, (tailDef "" restStr))
       '{' -> (extracted, (tailDef "" restStr))
       '[' -> (extracted, (tailDef "" restStr))
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

processString :: (String, String) -> (String, String)
processString (result, "") = (tailDef "" result, "")
processString (processed, restStr) =
  let (extracted, newRestStr) = extractPortion "" restStr
  in processString (processed ++ "," ++ (makePropertyTuple extracted), newRestStr)

simpleParamsToJson :: String -> String
simpleParamsToJson sparams = 
  let s = replace " " "" sparams
      (processed, _) = processString ("", s)
  in "{" ++ processed ++ "}"
