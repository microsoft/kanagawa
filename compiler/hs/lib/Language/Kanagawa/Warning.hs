{-# LANGUAGE DeriveDataTypeable #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Warning
  ( WarningKind(..)
  , WarningState(..)
  , enableSwitch
  , disableSwitch
  , enabledWarnings
  ) where

import Data.Data
import Data.Set (Set)
import qualified Data.Set as Set

data WarningKind =
     Conversion |
     Deprecated |
     InitializerOverrides |
     RedundantImport |
     Shadow |
     TransactionSizeMissing
     deriving (Eq, Ord, Data, Typeable)

instance Show WarningKind where
    show Conversion = "conversion"
    show Deprecated = "deprecated"
    show InitializerOverrides = "initializer-overrides"
    show TransactionSizeMissing = "transaction-size"
    show RedundantImport = "redundant-import"
    show Shadow = "shadow"

data WarningState a = Enable a | Disable a
     deriving (Eq, Ord, Show, Data, Typeable)

enableSwitch :: WarningKind -> String
enableSwitch = ("W" ++) . show

disableSwitch :: WarningKind -> String
disableSwitch = ("Wno-" ++) . show

enabledWarnings :: [WarningState WarningKind] -> Set WarningKind
enabledWarnings = foldl (flip update) mempty
  where
    update (Enable a) = Set.insert a
    update (Disable a) = Set.delete a

