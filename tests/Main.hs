{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Hastache 
import Text.Hastache.Context

import qualified Trurl as T
import qualified SimpleParams as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [trurlTests, simplParamsTests]

trurlTests :: TestTree
trurlTests = testGroup "Trurl unit tests"
  [ testCase "cutExtension" $
      assertEqual "Checking file name without extension" "a/b" (T.cutExtension "a/b.hs" ".hs")

  , testCase "cutSuffix" $
      assertEqual "Checking file name with suffix" "projectName" (T.cutSuffix "Bench.hs" "projectNameBench.hs")

  , testCase "cutSuffix" $
      assertEqual "Checking file name without suffix" "projectNameBench.hs" (T.cutSuffix "Bench.hs123" "projectNameBench.hs")

  , testCase "extractFileNameFromPath" $
      assertEqual "Checking file name in directory" "my.hs" (T.extractFileNameFromPath "e/f/my.hs")
      
  , testCase "extractFileNameFromPath" $
      assertEqual "Checking file name without directory" "my.hs" (T.extractFileNameFromPath "my.hs")
      
  , testCase "getFullFileName" $
      assertEqual "Checking full template path" "a/b.hs" (T.getFullFileName "a/" "b")

  , testCase "getFileName" $
      assertEqual "Checking file name" "abc.hs" (T.getFileName "abc")

  , testCase "getFileName" $
      assertEqual "Checking file name" "abc.html" (T.getFileName "abc.html")

  , testCase "mkContext empty" $ do
      generated <- hastacheStr defaultConfig "" (mkStrContext (T.mkContext "{\"a\":11}"))
      assertEqual "Checking generated text" "" generated

  , testCase "mkContext absent variable" $ do
      generated <- hastacheStr defaultConfig "{{b}}" (mkStrContext (T.mkContext "{\"a\":11}"))
      assertEqual "Checking generated text" "" generated

  , testCase "mkContext simple object" $ do
      generated <- hastacheStr defaultConfig "{{a}}" (mkStrContext (T.mkContext "{\"a\":11}"))
      assertEqual "Checking generated text" "11" generated

  , testCase "mkContext complex object" $ do
      generated <- hastacheStr defaultConfig "{{a}}-{{b}}" (mkStrContext (T.mkContext "{\"a\":11,\"b\":\"abc\"}"))
      assertEqual "Checking generated text" "11-abc" generated

  , testCase "mkContext complex array" $ do
      generated <- hastacheStr defaultConfig "{{#abc}}{{name}}{{/abc}}" (mkStrContext (T.mkContext "{\"abc\":[{\"name\":\"1\"},{\"name\":\"2\"},{\"name\":\"3\"}]}"))
      assertEqual "Checking generated text" "123" generated

  , testCase "mkContext nested object" $ do
      generated <- hastacheStr defaultConfig "{{#abc}}{{name}}{{/abc}}" (mkStrContext (T.mkContext "{\"abc\":{\"name\":\"1\"}}"))
      assertEqual "Checking generated text" "1" generated

  , testCase "mkProjContext for empty params" $ do
      generated <- hastacheStr defaultConfig "{{ProjectName}}" (mkStrContext (T.mkProjContext "abc" "{}"))
      assertEqual "Checking generated text" "abc" generated
  ]


simplParamsTests :: TestTree
simplParamsTests = testGroup "Trurl unit tests"
  [ testCase "extractPortion without delimiter" $
      assertEqual "Checking extractPortion" (S.ParsedObject "abc" "") (S.extractPortion "" "abc")

  , testCase "extractPortion with delimiter" $
      assertEqual "Checking extractPortion" (S.ParsedObject "abc" "efg,123") (S.extractPortion "" "abc,efg,123")

  , testCase "makePropertyTuple" $
      assertEqual "Checking makePropertyTuple" "\"abc\":\"efg\"" (S.makePropertyTuple "abc:efg")

  , testCase "makeObjectTuple" $
      assertEqual "Checking makeObjectTuple" "{\"name\":abc\",\"type\":\"efg\"}" (S.makeObjectTuple "abc#efg")

  , testCase "processString simple" $
      assertEqual "Checking processString" (S.ParsedObject "\"abc\":\"efg\",\"123\":\"456\"" "") (S.processString $ S.ParsedObject "" "abc:efg,123:456")

  ]
