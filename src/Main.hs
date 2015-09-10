{-# LANGUAGE OverloadedStrings #-}

module Main where

import Trurl
import SimpleParams

import System.Environment

help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <name> <project_template> -j [parameters_string] -- create project of specified type with specified name; optionally add JSON parameters, wrap it with \"\" or ''"
  putStrLn "  create <name> <project_template> [parameters] -- create project of specified type with specified name; optionally add parameters"
  putStrLn "  new <name> <file_template> -j [parameters_string] -- create file from the template with specified JSON parameters, wrap it with \"\" or ''"
  putStrLn "  new <name> <file_template> [parameters] -- create file from the template with specified string parameters"
  putStrLn "  list -- print all available templates"
  putStrLn "  help <template> -- print template info"
  putStrLn "  help -- print this help"
  putStrLn "  version -- print version"

printVersion :: IO ()
printVersion = putStrLn "0.3.0.x"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                                      -> help
    ["help"]                                -> help
    ["help", template]                      -> helpTemplate template
    ["update"]                              -> updateFromRepository
    ["create", name, project]               -> createProject name project "{}"
    ["create", name, project, "-j", params] -> createProject name project params
    ("create": name: project: params)       -> createProject name project $ simpleParamsToJson $ unwords params
    ["new", name, template, "-j" ,params]   -> newTemplate name template params
    ("new": name: template: params)         -> newTemplate name template $ simpleParamsToJson $ unwords params
    ["list"]                                -> listTemplates
    ["version"]                             -> printVersion
    ["-v"]                                  -> printVersion
    _                                       -> putStrLn "Unknown command"
