{-# LANGUAGE OverloadedStrings #-}

module Trurl where

import System.Directory
import System.FilePath
import Network.HTTP.Conduit
import Codec.Archive.Tar
import Data.List hiding (find)
import Text.Hastache
import Text.Hastache.Aeson
import Data.Aeson
import Data.String.Utils
import System.FilePath.Find (find, always, fileName, extension, (==?), liftOp)
import Safe
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8

import Registry

constProjectName :: String
constProjectName = "ProjectName"

registryUrl :: String
registryUrl = "https://github.com/dbushenko/trurl/raw/master/repository/registry.json"

templateExt :: String
templateExt = ".template"

getLocalRepoDir :: IO FilePath
getLocalRepoDir = do
  home <- getHomeDirectory
  return $ home ++ "/.trurl/repo/"

printFile :: FilePath -> FilePath -> IO ()
printFile dir fp = do
  file <- readFile (dir ++ fp)
  putStrLn file

printFileHeader :: FilePath -> FilePath -> IO ()
printFileHeader dir fp = do
  file <- readFile (dir ++ fp)
  putStrLn $ headDef "No info found..." $ lines file

processTemplate :: String -> String -> FilePath -> IO ()
processTemplate projName paramsStr filePath  = do
  generated <- hastacheFile defaultConfig filePath (mkProjContext projName paramsStr)
  TL.writeFile (dropExtension filePath) generated
  removeFile filePath
  return ()

getFileName :: FilePath -> FilePath
getFileName template =
  if hasExtension template
    then template
    else template <.> "hs"

getFullFileName :: FilePath -> String -> FilePath
getFullFileName repoDir template = repoDir ++ getFileName template

mkJsonContext :: Monad m => String -> MuContext m
mkJsonContext =
  maybe mkEmptyContext jsonValueContext . decode . BLC8.pack

mkEmptyContext :: Monad m => MuContext m
mkEmptyContext = const $ return MuNothing

mkProjContext :: Monad m => String -> String -> MuContext m
mkProjContext projName paramsStr =
  assoc "ProjectName" projName $ mkJsonContext paramsStr

mkFileContext :: Monad m => FilePath -> String -> MuContext m
mkFileContext fname paramsStr =
  assoc "FileName" fname $ mkJsonContext paramsStr

assoc :: (Monad m, MuVar a) => T.Text -> a -> MuContext m -> MuContext m
assoc newKey newVal oldCtx k =
  if k == newKey
    then return $ MuVariable newVal
    else oldCtx k

substituteProjectName :: String -> FilePath -> FilePath
substituteProjectName projectName filePath  =
  let (dirName, oldFileName) = splitFileName filePath
      newFileName = replace constProjectName projectName oldFileName
   in dirName </> newFileName

downloadTemplate :: String -> Registry -> IO ()
downloadTemplate repoDir (Registry url tname mname) = do
    let tFile = repoDir ++ tname
        mFile = repoDir ++ mname
    simpleHttp (url ++ tname) >>= BL.writeFile tFile
    simpleHttp (url ++ mname) >>= BL.writeFile mFile

-------------------------------------
-- API
--

-- Команда "update"
-- 1) Создать $HOME/.trurl/repo
-- 2) Забрать из репозитория реестр шаблонов
-- 3) Распарсить реестр как json
-- 4) Для каждого элемента реестра скачать шаблон и metainfo
--
updateFromRepository :: IO ()
updateFromRepository = do
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True repoDir
  regFile <- simpleHttp registryUrl
  case eitherDecode regFile :: Either String [Registry] of
      Left msg -> putStrLn $ "Can't parse registry file!\n" ++ msg
      Right registry -> mapM_ (downloadTemplate repoDir) registry
  
-- Команда "create <name> <project> [parameters]"
-- 1) Найти в $HOME/.trurl/repo архив с именем project.tar
-- 2) Создать директорию ./name
-- 3) Распаковать в ./name содержимое project.tar
-- 4) Найти все файлы с расширением ".template"
-- 5) Отрендерить эти темплейты c учетом переданных parameters
-- 6) Сохранить отрендеренные файлы в новые файлы без ".template"
-- 7) Удалить все файлы с расширением ".template"
-- 8) Найти все файлы с именем ProjectName независимо от расширения
-- 9) Переименовать эти файлы в соотествии с указанным ProjectName
--
createProject :: String -> String -> String -> IO ()
createProject name project paramsStr = do
  -- Extract the archive
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True name
  extract name $ repoDir ++ project ++ ".tar"

  -- Process all templates
  templatePaths <- find always (extension ==? templateExt) name
  mapM_ (processTemplate name paramsStr) templatePaths

  -- Find 'ProjectName' files
  let checkFileName fname templname = isInfixOf templname fname
  projNamePaths <- find always (liftOp checkFileName fileName constProjectName) name

  -- Rename 'ProjectName' files
  let renameProjNameFile fpath = renameFile fpath (substituteProjectName name fpath)
  mapM_ renameProjNameFile projNamePaths


-- Команда "new <file> [parameters]"
-- 1) Найти в $HOME/.trurl/repo архив с именем file.hs.
--    Если имя файла передано с расширением, то найти точное имя файла, не подставляя *.hs
-- 2) Прочитать содержимое шаблона
-- 3) Отрендерить его с применением hastache и переданных параметров
-- 4) Записать файл в ./
--
newTemplate :: String -> String -> String -> IO ()
newTemplate name templateName paramsStr = do
  repoDir <- getLocalRepoDir
  let templPath = getFullFileName repoDir templateName
  generated <- hastacheFile defaultConfig templPath (mkFileContext name paramsStr)
  TL.writeFile (getFileName name) generated

-- Команда "list"
-- 1) Найти все файлы с расширением '.metainfo'
-- 2) Для каждого найденного файла вывести первую строчку
--
listTemplates :: IO ()
listTemplates = do
  repoDir <- getLocalRepoDir
  files <- getDirectoryContents repoDir
  let mpaths = filter (endswith ".metainfo") files
  mapM_ (printFileHeader repoDir) mpaths

-- Команда "help <template>"
-- 1) Найти указанный файл с расширением '.metainfo'
-- 2) Вывести его содержимое
--
helpTemplate :: String -> IO ()
helpTemplate template = do
  repoDir <- getLocalRepoDir
  templExists <- doesFileExist $ repoDir ++ template ++ ".metainfo"
  if templExists then printFile repoDir $ template ++ ".metainfo"
  else printFile repoDir ((getFileName template) ++ ".metainfo")
