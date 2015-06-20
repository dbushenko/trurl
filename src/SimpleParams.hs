{-# LANGUAGE OverloadedStrings #-}

module SimpleParams where

import Data.List hiding (find)
import Data.String.Utils
import Safe
import Data.Char

parseEmbedded :: String -> String
parseEmbedded str =
  let splitted = split "#" str
  in if length splitted /= 2 then ""
     else if endswith "@" (splitted !! 1) then "{\"name\":\"" ++ splitted !! 0 ++ "\",\"type\":\"" ++ (replace "@" "" $ splitted !! 1) ++ "\",\"last\":true}"
          else "{\"name\":\"" ++ splitted !! 0 ++ "\",\"type\":\"" ++ splitted !! 1 ++ "\"}"

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
  in "{" ++  (foldl (++) "" $ map processPart  $ splitWs s) ++ "}"

