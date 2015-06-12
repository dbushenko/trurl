{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Hastache 
import Text.Hastache.Context

import qualified Trurl as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "cutExtension" $
      assertEqual "Checking file name without extension" "a/b" (T.cutExtension "a/b.hs" ".hs")

  , testCase "cutAnyExtension" $
      assertEqual "Checking file name with unknown extension" "a.b.c" (T.cutAnyExtension "a.b.c.hs")

  , testCase "cutAnyExtension" $
      assertEqual "Checking file name without any extension" "abc" (T.cutAnyExtension "abc")

  , testCase "extractAnyExtension" $
      assertEqual "Checking file name with '.hs' extension" "hs" (T.extractAnyExtension "e.f.abc.hs")
      
  , testCase "extractAnyExtension" $
      assertEqual "Checking file name without any extension" "" (T.extractAnyExtension "abc")
      
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
      generated <- hastacheStr defaultConfig "{{projectName}}" (mkStrContext (T.mkProjContext "abc" "{}"))
      assertEqual "Checking generated text" "abc" generated
  ]
