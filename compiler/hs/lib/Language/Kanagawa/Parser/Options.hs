{-# LANGUAGE DeriveDataTypeable #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Options
    ( ParseOptions(..)
    , Backend(..)
    , defaultOptions
    ) where

import Data.Data (Data, Typeable)

data Backend = Sv | None
      deriving (Eq, Show, Data, Typeable)

data ParseOptions = ParseOptions
    { baseLibrary       :: FilePath
    , define            :: [String]
    , files             :: [FilePath]
    , importDir         :: [FilePath]
    , noImplicitBase    :: Bool
    , parseDocs         :: Bool
    , optimize          :: Int
    , stall             :: Int
    , backend           :: Backend
    , targetDevice      :: String
    , maxThreadsLimit   :: Int
    , maxThreadsDefault :: Int
    , using             :: [String]
    }
  deriving (Eq, Show)

defaultOptions :: ParseOptions
defaultOptions = ParseOptions
    { baseLibrary       = "base.k"
    , define            = mempty
    , files             = mempty
    , importDir         = mempty
    , noImplicitBase    = False
    , parseDocs         = False
    , optimize          = -1
    , stall             = 0
    , backend           = None
    , targetDevice      = "mock"
    , maxThreadsLimit   = 512
    , maxThreadsDefault = 512
    , using             = mempty
    }
