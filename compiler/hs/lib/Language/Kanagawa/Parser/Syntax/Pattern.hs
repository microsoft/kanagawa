{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Syntax.Pattern
    ( module Language.Kanagawa.Parser.Syntax.Pattern
    ) where

import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.TH

$(deriveUnidirPatternSynAndCompletePragma 'Note ''ExpF)
