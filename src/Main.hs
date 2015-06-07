{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import Network.HTTP.Conduit
import Codec.Archive.Tar
import Data.List
import Text.Hastache 
import Text.Hastache.Context
import Data.Aeson
import Data.Maybe
import Data.Scientific

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.HashMap.Strict as HM

mainRepoFile :: String
mainRepoFile = "mainRepo.tar"

mainRepo :: String
mainRepo = "https://github.com/dbushenko/trurl/raw/master/repository/" ++ mainRepoFile

getLocalRepoDir :: IO String
getLocalRepoDir = do
  home <- getHomeDirectory
  return $ home ++ "/.trurl/repo/"
  

-- Команда "update"
-- 1) Создать $HOME/.trurl/repo
-- 2) Забрать из репозитория свежий tar-архив с апдейтами
-- 3) Распаковать его в $HOME/.trurl/repo

updateFromRepository :: IO ()
updateFromRepository = do
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True repoDir
  let tarFile = repoDir ++ mainRepoFile
  simpleHttp mainRepo >>= BL.writeFile tarFile
  extract repoDir tarFile
  removeFile tarFile


-- Команда "create <project> <name>"
-- 1) Найти в $HOME/.trurl/repo архив с именем project.tar
-- 2) Создать дирректорию ./name
-- 3) Распаковать в ./name содержимое project.tar

createProject :: String -> String -> IO ()
createProject project name = do
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True name
  extract name $ repoDir ++ project ++ ".tar"

-- Команда "new <file> <parameters>"
-- 1) Найти в $HOME/.trurl/repo архив с именем file.hs.
--    Если имя файла передано с расширением, то найти точное имя файла, не подставляя *.hs
-- 2) Прочитать содержимое шаблона
-- 3) Отрендерить его с применением hastache и переденных параметров
-- 4) Записать файл в ./

getFileName :: String -> String
getFileName template = do
  if (isInfixOf "." template) then template
  else template ++ ".hs"

getFullFileName :: String -> String -> String
getFullFileName repoDir template = repoDir ++ (getFileName template)

mkVariable :: Maybe Value -> MuType m
mkVariable (Just (String s)) = MuVariable s
mkVariable (Just (Bool b)) = MuBool b
mkVariable (Just (Number n)) = let e = floatingOrInteger n
                                   mkval (Left r) = MuVariable (r :: Double)
                                   mkval (Right i) = MuVariable (i :: Integer)
                               in mkval e


-- mkVariable (Just (Array ar)) = MuList $ map (mkStrContext . aesonContext . Just) (toList ar)
mkVariable (Just (Array _)) = MuVariable ("" :: String)
mkVariable (Just (Object _)) = MuVariable ("" :: String)
mkVariable (Just Null) = MuVariable ("" :: String)                               
mkVariable Nothing = MuVariable ("" :: String)

aesonContext :: Maybe Value -> String -> MuType m
aesonContext mobj = \k -> let obj = fromJust mobj
                              Object o = obj
                              v = HM.lookup (T.pack k) o
                         in mkVariable v

mkContext :: String -> (String -> MuType m)
mkContext paramsStr =
  let mobj = decode (BLC8.pack paramsStr) :: Maybe Value
  in if (isNothing mobj) then \_ -> MuVariable ("" :: String)
     else aesonContext mobj

newTemplate :: String -> String -> IO ()
newTemplate templateName paramsStr = do
  repoDir <- getLocalRepoDir
  let templPath = getFullFileName repoDir templateName
  template <- T.readFile templPath
  generated <- hastacheStr defaultConfig template (mkStrContext (mkContext paramsStr))
  TL.writeFile (getFileName templateName) generated

-- Хорошо бы сделать еще команду "list".
-- Для каждой команды должен быть файл с metainfo, по команде list будет выводиться
-- содержимое именно этих файлов
help :: IO ()
help = do
  putStrLn "trurl <command> [parameters]"
  putStrLn "  update -- fetch the updates from repository"
  putStrLn "  create <project_type> <name> -- create project of specified type with specified name"
  putStrLn "  new <template> <parameters_string> -- create file from the template with specified parameters, wrap it with \"\""
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
