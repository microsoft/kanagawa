{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.SymbolTable (
      Symbols
    , SymbolTable(..)
    , Scope(..)
    , Module(..)
    , Needed(..)
    , Import(..)
    , ExpSrc
    , emptySymbolTable
    , getScopeQualifier
    , addImport
    , updateImports
    , updateModule
    , updateVisible
    , parseScoped
    , parseSymbol
    , makeSymbolName
    , findSymbol
    , findInImport
    ) where

import Control.Monad.State.Lazy
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Symbols
import Language.Kanagawa.Recursion
import Text.Megaparsec

-- ExpF annotated with Src providing source information
type ExpSrc = NotedExp Src (ParseError String (ExtendedError String))

type Symbols = SymbolMap Src (ParseError String (ExtendedError String))

-- parser state, mutable and global
data SymbolTable =
    SymbolTable
        { scopes :: [Scope]                         -- symbols defined in currently visible scopes
        , module_ :: Module                         -- current module description
        }
    deriving (Show)

data Scope =
    Scope
        { scopeQualifier :: QualifiedName
        , symbolsInScope :: HashMap Name ExpSrc     -- symbols defined in the scope
        , lambdaIndex :: Int
        , scopeIndex :: Int
        , exportIndex :: Int
        }
    deriving (Show)

data Module =
    Module
        { namespace :: Name                         -- module namespace
        , moduleNeeded :: Needed                    -- `Needed` if import of the module is never redundant
        , imports :: [Import]                       -- imported modules
        , symbols :: Symbols                        -- symbols defined in the module
        , visibleSymbols :: Map Name Symbols        -- symbols exposed and reexposed by the module
        , exposedSymbols :: Map Name ExpSrc         -- exposed symbol declarations
        , exposedModules :: Set Name                -- exposed module names
        , exposedDiffs :: [(Name, Name)]            -- exposed module differences
        }
    deriving (Show)

data Import =
    Import
        { importedModule :: Module
        , importedAs :: Maybe Name
        , importOffset :: Int
        , importNeeded :: Needed
        }
    deriving (Show)

data Needed = Needed | Redundant
    deriving (Show)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Scope mempty mempty 0 0 0] (Module "" Redundant mempty mempty mempty mempty mempty mempty)

getScopeQualifier :: MonadState SymbolTable m => m QualifiedName
getScopeQualifier = gets (scopeQualifier . head . scopes)

updateImports :: MonadState SymbolTable m => Int -> Maybe Name -> SymbolTable -> m ()
updateImports offset qualifier t = modify addImport'
  where
    addImport' x@(SymbolTable _ m) = addImport needed offset qualifier x t
      where
        needed | namespace (module_ t) `Set.member` exposedModules m = Needed
               | any (namespace (module_ t) `in2`) $ exposedDiffs m  = Needed
               | otherwise                                           = moduleNeeded $ module_ t
        in2 a (b, c) = a == b || a == c

addImport :: Needed -> Int -> Maybe Name -> SymbolTable -> SymbolTable -> SymbolTable
addImport needed offset qualifier x t = x
    { module_ = (module_ x) { imports = Import (module_ t) qualifier offset needed : imports (module_ x) }
    }

updateModule :: MonadState SymbolTable m => ExpSrc -> [ExpSrc] -> m ()
updateModule (ModuleIdentifier n) exposed = modify $ \x -> x
    { module_ = (module_ x)
                    { namespace = moduleNamespace n
                    , exposedModules = Set.fromList $ mapMaybe exposedModule exposed
                    , exposedSymbols = Map.fromList $ mapMaybe exposedSymbol exposed
                    , exposedDiffs = mapMaybe exposedDiff exposed
                    }
    }
  where
    exposedModule (ModuleIdentifier a) = Just $ moduleNamespace a
    exposedModule _                    = Nothing

    exposedDiff (ModuleDiff a b)      = Just (moduleNamespace a, moduleNamespace b)
    exposedDiff _                     = Nothing

    exposedSymbol e@(Identifier a)    = Just (a, e)
    exposedSymbol _                   = Nothing

updateModule _ _ = undefined

updateVisible :: SymbolTable -> SymbolTable
updateVisible t@SymbolTable{..} = t { module_ = update module_ }
  where
    symbolName = head . tail

    visibleImported = visibleSymbols . importedModule

    namespaceImported = namespace . importedModule

    difference a b = Map.map (HashMap.filterWithKey
        (\k _ -> not $ getAny $ Map.foldMapWithKey
            (\namespace symbols -> Any ([namespace, symbolName k] `HashMap.member` symbols)) b)) a

    update m@Module{..} = m { visibleSymbols = Map.unionsWith HashMap.union [exposed, reExposed, diffs]
                            , moduleNeeded = if any exported symbols then Needed else Redundant
                            }
      where
        diffs = Map.unionsWith HashMap.union $ map diff exposedDiffs
        diff (n1, n2) = difference (moduleSymbols n1) (moduleSymbols n2)
        moduleSymbols n
            | namespace == n = Map.singleton namespace $ HashMap.filter exposable symbols
            | otherwise      = visibleImported $ fromJust $ find ((n ==) . namespaceImported) imports

        filterImported i@Import{..}
            | namespaceImported i `Set.member` exposedModules = visibleImported i
            | isJust importedAs                               = mempty
            | otherwise                                       = Map.map filterExposed $ visibleImported i

        filterExposed = HashMap.filterWithKey $
            \k _ -> symbolName k `Map.member` exposedSymbols

        reExposed = Map.filter (not . HashMap.null) $ Map.unionsWith HashMap.union $ map filterImported imports

        exposable (Variable _ _ typ _ _ _) = isConst typ
        exposable _ = True

        exposed
            | namespace `Set.member` exposedModules = Map.singleton namespace $ HashMap.filter exposable symbols
            | otherwise                             = Map.singleton namespace $ filterExposed symbols

        exported ExportType{} = True
        exported (Function _ _ attr _ _ _ _ _)
            | any reset $ unfix attr  = True
        exported _ = False

        reset (FlagAttr Reset) = True
        reset _                = False

parseScoped :: (MonadState SymbolTable m) =>
  (SymbolTable -> Int) -> (SymbolTable -> Int) -> ([Scope] -> [Scope]) -> [ExpSrc] -> Maybe ExpSrc -> m b -> m b
parseScoped startLambdaIndex startScopeIndex updateScopes symbols s p = do
    parentQualifier <- getScopeQualifier
    let scopeQualifier = parentQualifier ++ concatMap getQualifiedName s
        symbolsInScope = HashMap.fromList $ map (\x -> (getName x, x)) symbols
    modify (\x -> x { scopes = Scope scopeQualifier symbolsInScope (startLambdaIndex x) (startScopeIndex x) 0 : scopes x })
    result <- p
    modify (\x -> x { scopes = updateScopes $ scopes x })
    return result

parseSymbol :: (MonadParsec e s m, MonadState SymbolTable m) => m ExpSrc -> m ExpSrc
parseSymbol p = do
    result <- p
    name <- symbolName result
    s <- gets (HashMap.lookup name . symbolsInScope . head . scopes)
    case s of
        Just e ->
            symbolError result $ "symbol redefinition. See previous declaration at " ++ symbolLoc e
        _ -> do
            modify $ \x -> x { scopes = addToScope name result (scopes x)
                             , module_ = addToModule name result (module_ x) (scopeQualifier $ head $ scopes x)
                             }
            return result
  where
    addToScope name x (y:ys) = y { symbolsInScope = HashMap.insert name x $ symbolsInScope y } : ys
    addToScope _ _ [] = undefined

    addToModule name x y qualifier = y { symbols = insertSymbol (qualifier ++ [name]) x (symbols y) }

    symbolName ExportType{} = makeSymbolName "$export" exportIndex $ \x y -> x { exportIndex = y }
    symbolName e = return $ getName e

makeSymbolName :: MonadState SymbolTable m =>
    String -> (Scope -> Int) -> (Scope -> Int -> Scope) -> m Name
makeSymbolName name getIndex setIndex = do
    i <- state $ \s -> ( getIndex $ head $ scopes s
                       , s { scopes = incrementIndex $ scopes s }
                       )
    return $ indexedName i name
  where
    incrementIndex (x:xs) = setIndex x (1 + getIndex x) : xs
    incrementIndex [] = undefined

findSymbol :: (MonadParsec e s m, MonadState SymbolTable m) => ExpSrc -> m (Maybe ExpSrc)
findSymbol e = do
    t <- get
    sym <- findInSymbolTable e t
    case nub sym of
        [x] -> return $ Just x
        []  -> return Nothing
        xs  -> do
            registerSymbolError e $ "symbol is ambiguous. It could refer to the definition at\n\t   "
                                    ++ intercalate "\n\tor " (map symbolLoc xs)
            return Nothing
  where
    -- if no qualifier for the symbols is specified, first look for it by name in
    -- the current scope or one of its ancestors, otherwise search only by qualified
    -- name in the global symbols and imported symbols
    findInSymbolTable s SymbolTable{..}
        | null $ getQualifier s = findInScopes scopes >>= checkImportQualifiers
        | otherwise             = findInSymbols
      where
        checkImportQualifiers [x] = do
            case find ((Just (getName s) == ) . importedAs) $ imports module_ of
                Just Import{..} -> registerFancyError importOffset $ "The import qualifier '" ++ (getName s) ++
                                                                     "' conflicts with the name of symbol defined at "
                                                                     ++ symbolLoc x
                _ -> return ()

            return [x]
        checkImportQualifiers xs = return xs

        findInScopes (Scope{..}:xs) = case maybeToList $ HashMap.lookup (getName s) symbolsInScope of
            [x] -> (x :) <$> findInImports x
            _   -> findInScopes xs
        findInScopes _ = findInSymbols

        findInSymbols = ((maybeToList $ lookupSymbol s (symbols module_)) <>) <$> findInImports s

        findInImports x = do
            let foundImported = map (findInImport x) $ imports module_
                foundUnique = zipWith (foldl' (\\)) foundImported (inits foundImported)
            modify (\t -> t { module_ = module_ { imports = zipWith updateNeeded foundUnique (imports module_) } })
            return $ concat foundUnique
          where
            updateNeeded [] m = m
            updateNeeded _  m = m { importNeeded = Needed }

findInImport :: ExpLike a => a -> Import -> [ExpSrc]
findInImport x Import{..} = mapMaybe (findExposed $ getQualifiedName x) $ Map.toList $ visibleSymbols importedModule
  where
    findExposed name (namespace, symbols) = flip lookupSymbolByName symbols =<< withNamespace name
      where
        withNamespace (n:ns)
            | n == namespace = Just $ n:ns
        withNamespace ns     = (namespace:) <$> stripPrefix (maybeToList importedAs) ns
