{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Options (
  readConfig,
  vars,
  skipGlobs,
  passthroughGlobs,
  getFilters,
) where

import Render (Filter (..))

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Options.Applicative hiding (empty)
import System.FilePath.Glob
import Data.Yaml
import Data.Yaml.Internal
import Data.Aeson.KeyMap hiding (map, foldl, foldr, filter, fromList)
import Data.Aeson.Key
-- import Data.Vector (Vector, toList)
import Data.Text (Text, unpack, pack)
import System.Exit (die)
import qualified Data.Bifunctor
import Data.Char (toLower)
import Data.Vector (fromList)

data Config = Config { globals :: Maybe Object
                     , variables :: Maybe (KeyMap (KeyMap [Value]))
                     , ignore :: Maybe [String]
                     , passthrough :: Maybe [String]
                     , filters :: Maybe (KeyMap [KeyMap FilePath])
              } deriving (Generic, Show)
instance FromJSON Config

readConfig :: IO Config
readConfig = getOpts >>= \x -> decodeFileWithWarnings (configFile x) >>= parseHandle (ignoreConfigWarnings x)
  where
    parseHandle :: Bool -> Either ParseException ([Warning], Maybe Config) -> IO Config
    parseHandle _ (Left parseE) = die $ prettyPrintParseException parseE
    parseHandle _ (Right ([], Nothing)) = die "Couldn't read the config"
    parseHandle _ (Right ([], Just config)) = return config
    parseHandle i (Right (warns, _)) | not i = die $ "Received the following warnings while parsing the config file:\n" ++
      foldl (\acc s -> acc ++ show s ++ "\n") "" warns

skipGlobs :: Config -> [FilePath] -> [FilePath]
skipGlobs config = filter $ not . matchGlobs ((fromMaybe [] $ ignore config) ++ (fromMaybe [] $ passthrough config))

passthroughGlobs :: Config -> [FilePath] -> [FilePath]
passthroughGlobs config = filter $ matchGlobs $ fromMaybe [] $ passthrough config

matchGlob :: FilePath -> String -> Bool
matchGlob fp s = match (compile s) fp

matchGlobs :: [String] -> FilePath -> Bool
matchGlobs strs fp = any (matchGlob fp) strs

-- variables :: Maybe [KeyMap (KeyMap Value)]
vars :: Config -> FilePath -> Object
vars config fp = lookupVars (maybe [] toList $ variables config) `union` fromMaybe empty (globals config)
  where
    lookupVars :: [(Key, KeyMap [Value])] -> KeyMap Value
    lookupVars = foldr
        (\(k, v) r -> if String (pack fp) `elem` getFiles v then r `union` singleton k (getData v) else r) empty
    getFiles :: KeyMap [Value] -> [Value]
    getFiles km = fromMaybe [] $ km !? "files"
    getData :: KeyMap [Value] -> Value
    getData km = Array $ fromList $ fromMaybe [] $ km !? "data"

getFilters :: Config -> FilePath -> Either String [Filter]
getFilters config fp = mapM toFilter $ Data.Aeson.KeyMap.foldrWithKey (\key val acc -> if matchGlob fp (toString key) then acc ++ concatMap toList val else acc) [] filterKeyMap
  where
    filterKeyMap = fromMaybe empty $ filters config
    toFilter :: (Key, FilePath) -> Either String Filter
    toFilter (s, fp) =
      case map toLower $ toString s of
        "json" -> Right $ JSONFilter fp
        "lua" -> Right $ LuaFilter fp
        _ -> Left "Filter types must be either 'JSON' or 'Lua'"

data Opts = Opts { ignoreConfigWarnings :: Bool
                 , configFile :: String
                 } deriving (Show)

getOpts :: IO Opts
getOpts = execParser $ info
            (Opts
            <$> switch
                 ( long "ignore-config-warnings"
                <> short 'i'
                <> help "Whether to ignore warnings while parsing the config")
            <*> option auto
                 ( long "config"
                <> value "span.yml"
                <> showDefault
                <> metavar "CONFIG_FILE"
                <> help "The configuration file to use")
            <**> helper)
             ( fullDesc
            <> progDesc "Generate a static site from Pandoc Markdown and DocTemplates"
            <> header "Span - a static site generator based on Pandoc")
