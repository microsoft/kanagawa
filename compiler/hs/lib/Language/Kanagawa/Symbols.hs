{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Symbols
  ( getSymbols
  , getSymbols'
  , getSymbols''
  , SymbolsConfig(..)
  , SymbolMap
  , insertSymbol
  , lookupSymbol
  , lookupInScope
  , lookupSymbolByName
  , hasErrorSymbol
  ) where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Language.Kanagawa.Desugar
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion

type SymbolMap n e = HashMap QualifiedName (NotedExp n e)

data SymbolsConfig = AllSymbols | NoTemplate | TemplateDecl
  deriving Eq

errorsName :: QualifiedName
errorsName = ["$errors"]

hasErrorSymbol :: SymbolMap n e -> Bool
hasErrorSymbol = HashMap.member errorsName

getSymbols :: [NotedExp n e] -> SymbolMap n e
getSymbols = getSymbols' AllSymbols

getSymbols' :: SymbolsConfig -> [NotedExp n e] -> SymbolMap n e
getSymbols' config = HashMap.unions . parallelChunksOf 6 HashMap.unions . parallel (getSymbols'' config)

getSymbols'' :: SymbolsConfig -> NotedExp n e -> SymbolMap n e
getSymbols'' config = HashMap.fromList . reverse . para update
  where
    update e@Template{}
        | TemplateDecl == config  = symbol e . const mempty
    update e@Struct{}             = symbol e . const mempty
    update e@Union{}              = symbol e . const mempty
    update e                      = symbol e . concat

    symbol e@Alias{}              = insert e
    symbol e@Capture{}            = insert e
    symbol e@Struct{}             = insert e
    symbol e@Union{}              = insert e
    symbol  (Template _ Typename) = id
    symbol e@Template{}
        | NoTemplate == config    = tail
        | otherwise               = insert e
    symbol e@Function{}           = insert e
    symbol e@FunctionDecl{}       = insert e
    symbol e@Enum{}               = insert e
    symbol e@EnumConstant{}       = insert e
    symbol e@Variable{}           = insert e
    symbol e@FuncParam{}          = insert e
    symbol e@Class{}              = insert e . insert' e [getName e, thisName]
    symbol e@Error{}              = insert' e errorsName
    symbol _                      = id

    insert' e n = ((getScope e ++ n, e) :)
    insert e    = insert' e [getName e]

insertSymbol :: QualifiedName -> NotedExp n e -> SymbolMap n e -> SymbolMap n e
insertSymbol = HashMap.insert

lookupSymbolByName :: QualifiedName -> SymbolMap n e -> Maybe (NotedExp n e)
lookupSymbolByName = HashMap.lookup

lookupSymbol :: ExpLike exp => exp -> SymbolMap n e -> Maybe (NotedExp n e)
lookupSymbol e = lookupInScope (getScope e) e

lookupInScope :: ExpLike exp => QualifiedName -> exp -> SymbolMap n e -> Maybe (NotedExp n e)
lookupInScope s e symbols = case lookupSymbolByName (s ++ getQualifiedName e) symbols of
    Just x           -> Just x
    Nothing
      | null s       -> Nothing
      | otherwise    -> lookupInScope (init s) e symbols

