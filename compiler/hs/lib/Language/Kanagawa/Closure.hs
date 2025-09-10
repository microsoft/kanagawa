{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Closure
    ( closureCalls
    ) where

import Data.Maybe
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Type

closureCalls :: DesugarAlgebra (Typed n) (ParseError s e)
closureCalls (FunctionCallF a (FunctionSpecifier _ b) c)
    | tIsClosure $ typeOf b = FunctionCallF a function (capture `cons` c)
  where
    new = flip newNote b
    function = freeFunctionSpecifier $ qualifiedIdentifier (getScope b) (tClosureFunction $ typeOf b) b
    capture = new $ CastF (new $ fromJust $ expFromType b $ tClosureCapture $ typeOf b) (new $ NamedValueF b)
closureCalls e = e
