{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Hastache

import qualified Trurl as T
import qualified SimpleParams as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [trurlTests, simplParamsTests]

trurlTests :: TestTree
trurlTests = testGroup "Trurl unit tests"
  [ testCase "mkJsonContext empty" $ do
      generated <- hastacheStr defaultConfig "" (T.mkJsonContext "{\"a\":11}")
      assertEqual "Checking generated text" "" generated

  , testCase "mkJsonContext absent variable" $ do
      generated <- hastacheStr defaultConfig "{{b}}" (T.mkJsonContext "{\"a\":11}")
      assertEqual "Checking generated text" "" generated

  , testCase "mkJsonContext simple object" $ do
      generated <- hastacheStr defaultConfig "{{a}}" (T.mkJsonContext "{\"a\":11}")
      assertEqual "Checking generated text" "11" generated

  , testCase "mkJsonContext complex object" $ do
      generated <- hastacheStr defaultConfig "{{a}}-{{b}}" (T.mkJsonContext "{\"a\":11,\"b\":\"abc\"}")
      assertEqual "Checking generated text" "11-abc" generated

  , testCase "mkJsonContext complex array" $ do
      generated <- hastacheStr defaultConfig "{{#abc}}{{name}}{{/abc}}" (T.mkJsonContext "{\"abc\":[{\"name\":\"1\"},{\"name\":\"2\"},{\"name\":\"3\"}]}")
      assertEqual "Checking generated text" "123" generated

  , testCase "mkJsonContext nested object" $ do
      generated <- hastacheStr defaultConfig "{{#abc}}{{name}}{{/abc}}" (T.mkJsonContext "{\"abc\":{\"name\":\"1\"}}")
      assertEqual "Checking generated text" "1" generated

  , testCase "mkProjContext for empty params" $ do
      generated <- hastacheStr defaultConfig "{{ProjectName}}" (T.mkProjContext "abc" "{}")
      assertEqual "Checking generated text" "abc" generated
  ]


simplParamsTests :: TestTree
simplParamsTests = testGroup "Trurl unit tests"
  [ testCase "parseEmbedded without delimiter" $
      assertEqual "Checking parseEmbedded" "" (S.parseEmbedded "abc")

  , testCase "parseEmbedded with delimiter" $
      assertEqual "Checking parseEmbedded" "{\"key\":\"abc\",\"value\":\"efg\"}" (S.parseEmbedded "abc#efg")

  , testCase "parseEmbedded with delimiter" $
      assertEqual "Checking parseEmbedded" "{\"key\":\"abc\",\"value\":\"efg\",\"last\":true}" (S.parseEmbedded "abc#efg@")

  , testCase "simpleParamsToJson" $
      assertEqual "Checking simpleParamsToJson"
                  "{\"abc\":123,\"efg\":456,\"zxc\":[1,2,3],\"ttt\":[{\"key\":\"abc\",\"value\":\"efg\"},{\"key\":\"hck\",\"value\":\"qwe\"},{\"key\":\"zxc\",\"value\":\"vbn\",\"last\":true}]}"
                  (S.simpleParamsToJson "abc:123,efg:456,zxc:[1,2,3],ttt:[abc#efg,hck#qwe,zxc#vbn@]")

  , testCase "simple params with a space" $
      assertEqual "Checking simple params with a space"
                  "{\"props\":[{\"key\":\"cover\",\"value\":\"Text\"}, {\"key\":\"year\",\"value\":\"Integer\",\"last\":true}]}"
                  (S.simpleParamsToJson "props:[cover#Text, year#Integer@]")

  ]
