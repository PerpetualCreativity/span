{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant return" #-}

module Main where

import Options
import Render

import qualified Data.Text.IO
import System.Exit (die)
import Control.Monad
import System.Directory
import System.FilePath
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  opts <- getOpts

  config <- readConfig (configFile opts) (ignoreConfigWarnings opts)

  passthrough <- passthroughGlobs config . map removeEnclosingFolder <$> walkDir "contents"

  contents <- skipGlobs config . map removeEnclosingFolder <$> walkDir "contents"
  templates <- map removeEnclosingFolder <$> walkDir "templates"
  when (null contents) $ die "The contents folder is empty (or doesn't exist)!"
  when (null templates) $ die "The templates folder is empty (or doesn't exist)!"
  unless (defaultName `elem` templates) $ die "The templates folder must contain, at least, a default.html file."

  let ct = compileTemplates templates
  let em = extensionMap config

  let renderInfo c = RenderInfo {
    contentMap = ct,
    templates = templates,
    filepath = c,
    context = vars config c,
    eitherFilters = getFilters config c,
    fileType = fromMaybe "markdown" $ em !? tail (takeExtension c)
  }
  let createDir fp = createDirectoryIfMissing True $ takeDirectory fp
  let createAndWrite fp text = createDir fp >> Data.Text.IO.writeFile fp text
  let createAndCopy infp outfp = createDir outfp >> copyFile infp outfp

  let outputDir = outputDirName opts

  mapM_ (\c -> renderFile (renderInfo c) >>= createAndWrite (outputDir </> c -<.> "html")) contents
  mapM_ (\p -> createAndCopy ("contents" </> p) (outputDir </> p)) passthrough
