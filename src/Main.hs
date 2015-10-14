{-# LANGUAGE OverloadedStrings #-}

module Main where

import Trurl
import SimpleParams

import System.Environment

help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  new project <name> <project_template> -j [parameters_string] -- create project of specified type with specified name; optionally add JSON parameters, wrap it with \"\" or ''"
  putStrLn "  new project <name> <project_template> [parameters] -- create project of specified type with specified name; optionally add parameters"
  putStrLn "  new file <name> <file_template> -j [parameters_string] -- create file from the template with specified JSON parameters, wrap it with \"\" or ''"
  putStrLn "  new file <name> <file_template> [parameters] -- create file from the template with specified string parameters"
  putStrLn "  list -- print all available templates"
  putStrLn "  help <template> -- print template info"
  putStrLn "  help -- print this help"
  putStrLn "  version -- print version"

printVersion :: IO ()
printVersion = putStrLn "0.4.0.x"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                                      -> help
    ["help"]                                -> help
    ["help", template]                      -> helpTemplate template
    ["update"]                              -> updateFromRepository
    ["new", "project", name, project]       -> createProject name project "{}"
    ["new", "project", name, project, "-j", params] -> createProject name project params
    ("new project": name: project: params)          -> createProject name project $ simpleParamsToJson $ unwords params
    ["new", "file", name, template, "-j" ,params]   -> newTemplate name template params
    ("new file": name: template: params)    -> newTemplate name template $ simpleParamsToJson $ unwords params
    ["list"]                                -> listTemplates
    ["version"]                             -> printVersion
    ["--version"]                           -> printVersion
    ["-v"]                                  -> printVersion
    _                                       -> putStrLn "Unknown command"
