{-# LANGUAGE OverloadedStrings #-}

module Main where

import Trurl
import SimpleParams

import System.Environment

help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <name> <project_template> -j [parameters_string] -- create project of specified type with specified name; optionally add JSON parameters"
  putStrLn "  create <name> <project_template> -s [parameters_string] -- create project of specified type with specified name; optionally add string parameters"
  putStrLn "  create <name> <project_template> [parameters_string] -- create project of specified type with specified name; optionally add string parameters"
  putStrLn "  new <name> <file_template> -j [parameters_string] -- create file from the template with specified JSON parameters, wrap it with \"\" or ''"
  putStrLn "  new <name> <file_template> -s [parameters_string] -- create file from the template with specified string parameters, wrap it with \"\" or ''"
  putStrLn "  new <name> <file_template> [parameters_string] -- create file from the template with specified string parameters, wrap it with \"\" or ''"
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
    ["create", name, project, "-s", params] -> createProject name project $ simpleParamsToJson params
    ["create", name, project, params]       -> createProject name project $ simpleParamsToJson params
    ["new", name, template, "-j" ,params]   -> newTemplate name template params
    ["new", name, template, "-s" ,params]   -> newTemplate name template $ simpleParamsToJson params
    ["new", name, template, params]         -> newTemplate name template $ simpleParamsToJson params
    ["list"]                                -> listTemplates
    ["version"]                             -> printVersion
    ["-v"]                                  -> printVersion
    _                                       -> putStrLn "Unknown command"
