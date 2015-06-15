{-# LANGUAGE OverloadedStrings #-}

module Main where

import Trurl
import System.Environment

help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <name> <project_template> [parameters_string] -- create project of specified type with specified name; optionally add JSON parameters"
  putStrLn "  new <name> <file_template> [parameters_string] -- create file from the template with specified parameters, wrap it with \"\""
  putStrLn "  list -- print all available templates"
  putStrLn "  help <template> -- print template info"
  putStrLn "  help -- print this help"
  putStrLn "  version -- print version"

printVersion :: IO ()
printVersion = putStrLn "0.3.0.0"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                                -> help
    ["help"]                          -> help
    ["help", template]                -> helpTemplate template
    ["update"]                        -> updateFromRepository
    ["create", name, project]         -> createProject name project "{}"
    ["create", name, project, params] -> createProject name project params
    ["new", name, template, params]   -> newTemplate name template params
    ["list"]                          -> listTemplates
    ["version"]                       -> printVersion
    ["-v"]                            -> printVersion
    _                                 -> putStrLn "Unknown command"
