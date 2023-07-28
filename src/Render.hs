{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant return" #-}

module Render (renderFile, compileTemplates, walkDir, removeEnclosingFolder, defaultName, Filter (..), RenderInfo (..)) where

import Text.DocTemplates
import Data.Text (Text, unpack)
import qualified Data.Text
import qualified Data.Text.IO
import Text.Pandoc
import Text.Pandoc.Lua (getEngine)
import Text.Pandoc.Filter
import Text.Pandoc.Readers (Reader(..))
import Text.Pandoc.Format (FlavoredFormat (..), ExtensionsDiff (..))
import Text.Pandoc.Extensions (Extensions, emptyExtensions)
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

data RenderInfo = RenderInfo {
  contentMap :: Map FilePath (IO (Template Text)),
  templates :: [FilePath],
  filepath :: FilePath,
  context :: Object,
  eitherFilters :: Either String [Filter],
  fileType :: String
}

renderFile :: RenderInfo -> IO Text
renderFile i = do
  let fp = filepath i
  let templatePath = recSearch (templates i) fp
  let contentPath = "contents" </> fp
  putStrLn $ "Rendering " ++ contentPath ++ " with template " ++ ("templates" </> templatePath) ++ "."
  template <- contentMap i Map.! templatePath
  file <- Data.Text.IO.readFile contentPath

  let exec x = runIO x >>= handleError

  (genReader, exts) <- exec $ getReader FlavoredFormat {
    formatName = Data.Text.pack $ fileType i,
    formatExtsDiff = ExtensionsDiff {
      extsToEnable = emptyExtensions,
      extsToDisable = emptyExtensions
    }
  }

  reader <- (case genReader of
        TextReader r -> return r
        ByteStringReader r -> die "Span currently doesn't support ByteStringReaders. Please create an issue if you want to use this Pandoc feature.")

  filters <- case eitherFilters i of
    Left m -> die m
    Right fs -> return fs
  scrngin <- exec getEngine

  -- We employ a workaround so that people can use Text.DocTemplates syntax
  -- in their content files.

  let vars = toContext $ Object (context i)

  let file1 = Data.Text.replace "%" "$" $ Data.Text.replace "$" "\US" file
  md1 <- exec $ reader (def {readerExtensions=exts}) file
  t1 <- compileTemplate ("contents" </> fp) file1 >>= checkTemplateCompilation "file" fp
  r1 <- exec $ writeMarkdown (def {writerTemplate=Just t1, writerVariables=vars}) md1

  let file2 = Data.Text.replace "\US" "$" r1

  md2 <- exec $ reader (def {readerExtensions=exts}) file2
  filtered <- exec $ applyFilters scrngin def filters [] md2
  exec $ writeHtml5String (def {writerTemplate=Just template, writerVariables=vars}) filtered
