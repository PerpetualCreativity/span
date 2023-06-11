{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE LambdaCase #-}

module Render (renderFile, compileTemplates, walkDir, removeEnclosingFolder, defaultName, Filter (..)) where

import Text.DocTemplates
import Data.Text (Text, unpack)
import qualified Data.Text
import qualified Data.Text.IO
import Text.Pandoc
import Text.Pandoc.Lua (getEngine)
import Text.Pandoc.Filter
import System.Directory
import System.FilePath
import System.Exit (die)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set
import Data.Yaml (Object, Value (Object))

defaultName = "default.html"

walkDir :: FilePath -> IO [FilePath]
walkDir fp = do
  exists <- doesPathExist fp
  isDir <- doesDirectoryExist fp
  if not exists then
    return []
  else if isDir then
    concat <$> (listDirectory fp >>= mapM (walkDir . (fp </>)))
  else
    return [fp]

recSearch :: [FilePath] -> FilePath -> FilePath
recSearch fps fp = let
    dir = takeDirectory fp
    sameDirDefault = dir </> defaultName
  in
  if fp `elem` fps then fp
  else if sameDirDefault `elem` fps then sameDirDefault
  else if takeDirectory dir == "." then defaultName
  else recSearch fps (takeDirectory dir </> takeFileName fp)

removeEnclosingFolder :: FilePath -> FilePath
removeEnclosingFolder = joinPath . tail . splitPath

compileTemplates :: [FilePath] -> Map FilePath (IO (Template Text))
compileTemplates fps = Map.fromSet (\fp -> compileTemplateFile ("templates" </> fp) >>= checkTemplateCompilation "template" fp) $ Data.Set.fromList fps

checkTemplateCompilation :: String -> FilePath -> Either String (Template a) -> IO (Template a)
checkTemplateCompilation t fp (Left e) = die $ "Error from Pandoc while compiling " ++ t ++ " " ++ fp ++ "):\n" ++ e
checkTemplateCompilation _ _ (Right t) = return t

renderFile :: Map FilePath (IO (Template Text)) -> [FilePath] -> FilePath -> Object -> Either String [Filter] -> IO Text
renderFile cmap templs fp ctxt eitherFilters = do
  let templatePath = recSearch templs fp
  let contentPath = "contents" </> fp
  putStrLn $ "Rendering " ++ contentPath ++ " with template " ++ ("templates" </> templatePath) ++ "."
  template <- cmap Map.! templatePath
  file <- Data.Text.IO.readFile $ "contents" </> fp

  let exts = getDefaultExtensions "markdown"
  let exec x = runIO x >>= handleError

  filters <- case eitherFilters of
    Left m -> die m
    Right fs -> return fs
  scrngin <- exec getEngine

  md <- exec $ readMarkdown (def {readerExtensions=exts}) file
  filtered <- exec $ applyFilters scrngin def filters [] md
  exec $ writeHtml5String (def {writerTemplate=Just template, writerVariables=toContext $ Object ctxt}) md
