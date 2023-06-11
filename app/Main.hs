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

main :: IO ()
main = do
  config <- readConfig
  print config

  contents <- ignoreGlobs config . map removeEnclosingFolder <$> walkDir "contents"
  templates <- map removeEnclosingFolder <$> walkDir "templates"
  when (null contents) $ die "The contents folder is empty (or doesn't exist)!"
  when (null templates) $ die "The templates folder is empty (or doesn't exist)!"
  unless (defaultName `elem` templates) $ die "The templates folder must contain, at least, a default.html file."

  let ct = compileTemplates templates

  let render c = renderFile ct templates c (vars config c) (getFilters config c)
  let createAndWrite fp text = createDirectoryIfMissing True (takeDirectory fp) >> Data.Text.IO.writeFile fp text

  mapM_ (\c -> render c >>= createAndWrite ("output" </> c -<.> "html")) contents

  return ()
