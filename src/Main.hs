{-# LANGUAGE OverloadedStrings #-}

module Main where

import Trurl
import System.Environment

help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <project_template> <name> -- create project of specified type with specified name"
  putStrLn "  new <name> <template> <parameters_string> -- create file from the template with specified parameters, wrap it with \"\""
  putStrLn "  list -- print all available templates"
  putStrLn "  help <template> -- print template info"
  putStrLn "  help -- print this help"
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    []                              -> help
    ["help"]                        -> help
    ["help", template]              -> helpTemplate template
    ["update"]                      -> updateFromRepository
    ["create", project, name]       -> createProject project name
    ["new", name, template, params] -> newTemplate name template params
    ["list"]                        -> listTemplates
    _                               -> putStrLn "Unknown command"
