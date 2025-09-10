{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle.Cli
    ( Cli(..)
    , Format(..)
    , cliToParseOptions
    ) where

import Data.Data
import qualified Language.Kanagawa.Parser.IO as P

data Cli = Cli
    { base_library     :: FilePath
    , debug            :: Bool
    , define           :: [String]
    , files            :: [FilePath]
    , format           :: Format
    , import_dir       :: [FilePath]
    , no_implicit_base :: Bool
    , output           :: FilePath
    , svgbob           :: Bool
    , target_device    :: String
    , using            :: [String]
    }
  deriving (Data, Eq, Read, Show, Typeable)

-- | Supported output formats
data Format = Html | Markdown
  deriving (Bounded, Data, Enum, Eq, Read, Show, Typeable)

cliToParseOptions :: Cli -> P.ParseOptions
cliToParseOptions Cli{..} = P.defaultOptions
    { P.baseLibrary    = base_library
    , P.define         = define
    , P.files          = files
    , P.importDir      = import_dir
    , P.noImplicitBase = no_implicit_base
    , P.parseDocs      = True
    , P.targetDevice   = target_device
    , P.using          = using
    }
