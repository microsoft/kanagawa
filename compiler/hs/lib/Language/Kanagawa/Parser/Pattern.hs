{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Pattern
    ( module Language.Kanagawa.Parser.Pattern
    ) where

import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.TH

$(deriveExplBidirPatternSyn ''ExpF)
