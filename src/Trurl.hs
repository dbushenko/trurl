{-# LANGUAGE OverloadedStrings #-}

module Trurl where

import GHC.Exts
import System.Directory
import Network.HTTP.Conduit
import Codec.Archive.Tar
import Data.List hiding (find)
import Text.Hastache
import Text.Hastache.Context
import Data.Aeson
import Data.Maybe
import Data.Scientific
import Data.String.Utils
import System.FilePath.Find (find, always, fileName, extension, (==?), liftOp)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.HashMap.Strict as HM

constProjectName :: String
constProjectName = "projectName"

mainRepoFile :: String
mainRepoFile = "mainRepo.tar"

mainRepo :: String
mainRepo = "https://github.com/dbushenko/trurl/raw/master/repository/" ++ mainRepoFile

templateExt :: String
templateExt = ".template"

getLocalRepoDir :: IO String
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
  putStrLn $ head $ split "\n" file

cutExtension :: String -> String -> String
cutExtension filePath ext = take (length filePath - length ext) filePath

cutAnyExtension :: String -> String
cutAnyExtension fname =
  let mn = elemIndex '.' $ reverse fname
      cutExt Nothing = fname
      cutExt (Just n)  = take ((length fname) - n - 1) fname
  in cutExt mn

extractAnyExtension :: String -> String
extractAnyExtension fname =
  let mn = elemIndex '.' $ reverse fname
      extractExt Nothing = ""
      extractExt (Just n) = drop ((length fname) - n) fname
  in extractExt mn

processTemplate :: String -> String -> String -> IO ()
processTemplate projName paramsStr filePath  = do
  template <- T.readFile filePath
  generated <- hastacheStr defaultConfig template (mkStrContext (mkProjContext projName paramsStr))
  TL.writeFile (cutExtension filePath templateExt) generated
  removeFile filePath
  return ()

getFileName :: String -> String
getFileName template =
  if "." `isInfixOf` template then template
  else template ++ ".hs"

getFullFileName :: String -> String -> String
getFullFileName repoDir template = repoDir ++ getFileName template

mkVariable :: Monad m => Value -> MuType m
mkVariable (String s) = MuVariable s
mkVariable (Bool b) = MuBool b
mkVariable (Number n) = let e = floatingOrInteger n
                            mkval (Left r) = MuVariable (r :: Double)
                            mkval (Right i) = MuVariable (i :: Integer)
                        in mkval e

mkVariable (Array ar) = MuList $ map (mkStrContext . aesonContext . Just) (toList ar)
mkVariable o@(Object _) = MuList [ mkStrContext $ aesonContext $ Just o ]
mkVariable Null = MuVariable ("" :: String)

aesonContext :: Monad m => Maybe Value -> String -> MuType m
aesonContext mobj k = let obj = fromJust mobj
                          Object o = obj
                          v = HM.lookupDefault Null (T.pack k) o
                      in mkVariable v

mkContext :: Monad m => String -> String -> MuType m
mkContext paramsStr =
  let mobj = decode (BLC8.pack paramsStr) :: Maybe Value
  in if isNothing mobj then \_ -> MuVariable ("" :: String)
     else aesonContext mobj

mkProjContext :: Monad m => String -> String -> String -> MuType m
mkProjContext projName _ "projectName" = MuVariable projName
mkProjContext _ paramsStr key             = mkContext paramsStr key

-------------------------------------
-- API
--

-- Команда "update"
-- 1) Создать $HOME/.trurl/repo
-- 2) Забрать из репозитория свежий tar-архив с апдейтами
-- 3) Распаковать его в $HOME/.trurl/repo
--
updateFromRepository :: IO ()
updateFromRepository = do
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True repoDir
  let tarFile = repoDir ++ mainRepoFile
  simpleHttp mainRepo >>= BL.writeFile tarFile
  extract repoDir tarFile
  removeFile tarFile

-- Команда "create <name> <project> [parameters]"
-- 1) Найти в $HOME/.trurl/repo архив с именем project.tar
-- 2) Создать директорию ./name
-- 3) Распаковать в ./name содержимое project.tar
-- 4) Найти все файлы с расширением ".template"
-- 5) Отрендерить эти темплейты c учетом переданных parameters
-- 6) Сохранить отрендеренные файлы в новые файлы без ".template"
-- 7) Удалить все файлы с расширением ".template"
-- 8) Найти все файлы с именем projectName независимо от расширения
-- 9) Переименовать эти файлы в соотествии с указанным projectName
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

  -- Find 'projectName' files
  let checkFileName fname templname = (cutAnyExtension fname) == templname
  projNamePaths <- find always (liftOp checkFileName fileName constProjectName) name

  -- Rename 'projectName' files
  let renameProjNameFile fname = let ext = extractAnyExtension fname
                                     fpath = cutExtension fname (constProjectName ++ "." ++ ext)
                                 in renameFile fname (fpath ++ name ++ "." ++ ext)
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
  template <- T.readFile templPath
  generated <- hastacheStr defaultConfig template (mkStrContext (mkContext paramsStr))
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
  printFile repoDir ((getFileName template) ++ ".metainfo")
