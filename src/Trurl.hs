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
import System.FilePath.Find (find, always, extension, (==?))

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

mkVariable :: Monad m => Maybe Value -> MuType m
mkVariable (Just (String s)) = MuVariable s
mkVariable (Just (Bool b)) = MuBool b
mkVariable (Just (Number n)) = let e = floatingOrInteger n
                                   mkval (Left r) = MuVariable (r :: Double)
                                   mkval (Right i) = MuVariable (i :: Integer)
                               in mkval e

mkVariable (Just (Array ar)) = MuList $ map (mkStrContext . aesonContext . Just) (toList ar)
mkVariable (Just o@(Object _)) = MuList [ mkStrContext $ aesonContext $ Just o ]
mkVariable (Just Null) = MuVariable ("" :: String)                               
mkVariable Nothing = MuVariable ("" :: String)

aesonContext :: Monad m => Maybe Value -> String -> MuType m
aesonContext mobj k = let obj = fromJust mobj
                          Object o = obj
                          v = HM.lookup (T.pack k) o
                      in mkVariable v

mkContext :: Monad m => String -> String -> MuType m
mkContext paramsStr =
  let mobj = decode (BLC8.pack paramsStr) :: Maybe Value
  in if isNothing mobj then \_ -> MuVariable ("" :: String)
     else aesonContext mobj

mkProjContext :: Monad m => String -> String -> String -> MuType m
mkProjContext projName _         "projName" = MuVariable projName
mkProjContext _        paramsStr key        = mkContext paramsStr key

-------------------------------------
-- API
--

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
-- 2) Создать директорию ./name
-- 3) Распаковать в ./name содержимое project.tar

createProject :: String -> String -> String -> IO ()
createProject name project paramsStr = do
  repoDir <- getLocalRepoDir
  createDirectoryIfMissing True name
  extract name $ repoDir ++ project ++ ".tar"
  templatePaths <- find always (extension ==? templateExt) name
  mapM_ (processTemplate name paramsStr) templatePaths

-- Команда "new <file> <parameters>"
-- 1) Найти в $HOME/.trurl/repo архив с именем file.hs.
--    Если имя файла передано с расширением, то найти точное имя файла, не подставляя *.hs
-- 2) Прочитать содержимое шаблона
-- 3) Отрендерить его с применением hastache и переданных параметров
-- 4) Записать файл в ./

newTemplate :: String -> String -> String -> IO ()
newTemplate name templateName paramsStr = do
  repoDir <- getLocalRepoDir
  let templPath = getFullFileName repoDir templateName
  template <- T.readFile templPath
  generated <- hastacheStr defaultConfig template (mkStrContext (mkContext paramsStr))
  TL.writeFile (getFileName name) generated

listTemplates :: IO ()
listTemplates = do
  repoDir <- getLocalRepoDir
  files <- getDirectoryContents repoDir
  let mpaths = filter (endswith ".metainfo") files
  mapM_ (printFileHeader repoDir) mpaths

helpTemplate :: String -> IO ()
helpTemplate template = do
  repoDir <- getLocalRepoDir
  printFile repoDir ((getFileName template) ++ ".metainfo")
