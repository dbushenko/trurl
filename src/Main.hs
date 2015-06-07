{-# LANGUAGE OverloadedStrings #-}

import System.Environment

mainRepo = "https://github.com/dbushenko/trurl/raw/master/repository/main.tar"

-- Команда "update"
-- 1) Забрать из репозитория свежий tar-архив с апдейтами
-- 2) Распаковать его в $HOME/.trurl/repo

updateFromRepository :: IO ()
updateFromRepository = return ()


-- Команда "create <project> <name>"
-- 1) Найти в $HOME/.trurl/repo архив с именем project.tar
-- 2) Создать дирректорию ./name
-- 3) Распаковать в ./name содержимое project.tar

createProject :: String -> String -> IO ()
createProject project name = return ()

-- Команда "new <file> <parameters>"
-- 1) Найти в $HOME/.trurl/repo архив с именем file.hs.
--    Если имя файла передано с расширением, то найти точное имя файла, не подставляя *.hs
-- 2) Прочитать содержимое шаблона
-- 3) Отрендерить его с применением hastache и переденных параметров
-- 4) Записать файл в ./

newTemplate :: String -> String -> IO ()
newTemplate template params = return ()


help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <project_type> <name> -- create project of specified type with specified name"
  putStrLn "  new <template> <parameters> -- create file from the template with specified parameters"
  putStrLn "  help -- print this help"
  

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                        -> help
    ["help"]                  -> help
    ["update"]                -> updateFromRepository
    ["create", project, name] -> createProject project name
    ["new", template, params] -> newTemplate template params
    _                         -> putStrLn "Unknown command"
