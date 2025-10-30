{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser (
      cmdargsSpecialModule
    , optionsSpecialModule
      -- * Parsers
    , parseKanagawa
    , ParseErrors
    , ParseResult
    , Cache
    , SymbolTable(..)
    , ExpSrc
    ) where

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.List hiding (union)
import Data.Maybe
import Prettyprinter (pretty)
import Language.Kanagawa.Desugar (makeQualified, copyNote, desugar, indexedName, functionScope)
import Language.Kanagawa.Error
import Language.Kanagawa.Parser.Lexer hiding (symbol)
import qualified Language.Kanagawa.Parser.Lexer as L
import Language.Kanagawa.Parser.Pattern
import Language.Kanagawa.Parser.SymbolTable
import Language.Kanagawa.Parser.Syntax
import System.FilePath (addExtension, joinPath)
import Text.Megaparsec hiding (some, many, sepBy, sepBy1, sepEndBy, sepEndBy1)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Text.Megaparsec.Char

cmdargsSpecialModule :: String
cmdargsSpecialModule = ".cmdargs"

optionsSpecialModule :: String
optionsSpecialModule = ".options"

type Parser' s e a = ReaderT Config (StateT SymbolTable (ParsecT e s Cache)) a

type Parser a = Parser' String (ExtendedError String) a

type ParseErrors = ErrorBundle String

type ParseResult = Either ParseErrors (ExpSrc, SymbolTable)

type Cache = StateT [(FilePath, ParseResult)] IO

-- Parser combinator which annotates results of a parser with source information
noted :: Parser ExpSrc -> Parser ExpSrc
noted parser = do
    offset         <- getOffset
    begin          <- getSourcePos
    (NotedExp _ e) <- parser
    end            <- getSourcePos
    return $ NotedExp (Src offset begin end mempty) e

unaryNoted :: Parser (ExpSrc -> ExpSrc) -> Parser (ExpSrc -> ExpSrc)
unaryNoted parser = do
    offset         <- getOffset
    begin          <- getSourcePos
    fn             <- parser
    end            <- getSourcePos
    return $ parser' offset begin end fn
  where
    parser' offset begin end fn x = NotedExp (Src offset begin end mempty) e
      where
        (NotedExp _ e) = fn x

binaryNoted :: Parser (ExpSrc -> ExpSrc -> ExpSrc) -> Parser (ExpSrc -> ExpSrc -> ExpSrc)
binaryNoted parser = do
    offset         <- getOffset
    begin          <- getSourcePos
    fn             <- parser
    end            <- getSourcePos
    return $ parser' offset begin end fn
  where
    parser' offset begin end fn x y = NotedExp (Src offset begin end mempty) e
      where
        (NotedExp _ e) = fn x y

-- Parser combinator which annotates results of a parser with source information and documentation annotations
notedWithDocs :: Parser ExpSrc -> Parser ExpSrc
notedWithDocs parser = do
    docPre         <- docCommentPre
    (NotedExp n e) <- noted parser
    docPost        <- docCommentPost
    return $ NotedExp (n {docs = (docPre, docPost)}) e

parseKanagawa ::
    (FilePath -> Cache (Either String ParseResult)) -- ^ function to parse file, used to handle imports
 -> Config                                          -- ^ parser configuration
 -> SymbolTable                                     -- ^ initial symbol table
 -> FilePath                                        -- ^ source name, used only for error messages
 -> String                                          -- ^ content of a Kanagawa file to parse
 -> Cache ParseResult                               -- ^ returns either `ParseErrorBundle` if parser failed
                                                    --   or 'ExpSrc', which represents the AST, and final SymbolTable
parseKanagawa parseFile config t = runParserT (runStateT (runReaderT parser config) t)
  where
    parser = between whitespace eof (program parseFile)

program :: (FilePath -> Cache (Either String ParseResult)) -> Parser ExpSrc
program parseFile = (moduleDecl <|> declarations) >>= checkExposed >>= checkRedundantImports
  where
    -- Parser combinators that wrap list in a noted Seq of noted expressions
    many p        = noted $ Seq <$> MP.many (notedWithDocs p)
    sepBy p s     = noted $ Seq <$> (notedWithDocs p `MP.sepBy` s)
    sepBy1 p s    = noted $ Seq <$> (noted p `MP.sepBy1` s)
    sepEndBy p s  = noted $ Seq <$> (notedWithDocs p `MP.sepEndBy` s)

    unscopedWith symbols = parseScoped (const 0) (const 0) tail symbols Nothing

    -- parser combinator executes a parser in a new named scope, e.g. class,
    -- with specified symbols defined for that scope
    scopedWith symbols = parseScoped (const 0) (const 0) tail symbols . Just

    scoped = scopedWith []

    functionScopedWith symbols = scopedWith symbols (Identifier "$")

    functionScoped = functionScopedWith []

    getQuasiScopeIdentifier = do
        scopeIndex <- gets (scopeIndex . head . scopes)
        return $ Identifier $ indexedName scopeIndex "$quasi"

    -- parser combinator executes a parser in a new unnamed scope, e.g. {}
    quasiScopedWith symbols p = do
        ident <- getQuasiScopeIdentifier
        parseScoped parentLambdaIndex parentScopeIndex updateParentIndices symbols (Just ident) p
      where
        parentLambdaIndex = lambdaIndex . head . scopes
        parentScopeIndex = (1 +) . scopeIndex . head . scopes
        updateParentIndices (x:y:xs) = y { lambdaIndex = lambdaIndex x, scopeIndex = scopeIndex y + 1 } : xs
        updateParentIndices _ = []

    quasiScoped = quasiScopedWith []

    localScoped p = do
        ident <- getQuasiScopeIdentifier
        parseScoped parentLambdaIndex parentScopeIndex updateParentIndices [] (Just ident) p
      where
        parentLambdaIndex = lambdaIndex . head . scopes
        parentScopeIndex = (1 +) . scopeIndex . head . scopes
        updateParentIndices (x:y:xs) = y { scopeQualifier = scopeQualifier y ++ [indexedName (scopeIndex y + 1) "$quasi"]
                                         , lambdaIndex = lambdaIndex x
                                         , scopeIndex = 0 } : xs
        updateParentIndices _ = []

    -- parser combinator adds result of a parser to symbol table
    symbol = parseSymbol . noted

    symbolWithDocs = parseSymbol . notedWithDocs

    attributes xs = Seq <$> doubleBrackets (choice xs `MP.sepBy1` commaOrBrackets) <?> "attribute specifier sequence"
      where
        commaOrBrackets = comma <|> try (L.symbol "]]" >> L.symbol "[[")

    emptySeq = pure $ Seq []

    optionalAttributes xs = try (attributes xs) <|> emptySeq

    flagAttr x = FlagAttr x <$ L.symbol (show x)

    intAttr x = IntAttr x <$ L.symbol (show x) <*> parens expression

    identAttr x = Attr x <$ L.symbol (show x) <*> parens qualifiedIdentifier

    strAttr x = Attr x <$ L.symbol (show x) <*> parens strLiteral

    moduleName = choice
        [ pure <$> L.symbol cmdargsSpecialModule
        , pure <$> L.symbol optionsSpecialModule
        , dotSepName
        ] <?> "module name"

    moduleDecl = do
        ident <- moduleIdentifier
        exposed <- braces ((moduleDiff <|> moduleIdentifier <|> notedWithDocs identifier) `MP.sepBy` comma)
        updateModule ident exposed
        noted ((Seq . pure) . ModuleDecl ident exposed <$> scoped ident declarations)
      where
        moduleIdentifier = try $ notedWithDocs (ModuleIdentifier <$ keyword "module" <*> moduleName <?> "module specifier")
        moduleDiff = try $ notedWithDocs (ModuleDiff <$ keyword "module" <*> moduleName <* L.symbol "\\" <*> moduleName <?> "module difference")

    parseImports = skipMany $ do
        offset <- getOffset
        ns <- keyword "import" *> moduleName
        as <- optional (keyword "as" *> name <?> "module alias")
        let parseFileWithExt = lift . lift . lift . parseFile . addExtension (joinPath ns)
        k <- parseFileWithExt "k"
        case k of
            Left err -> parseFileFailure offset err
            Right result -> either (importFailure offset) (updateSymbolTable offset ns as) result
      where
        updateSymbolTable offset ns as (_, t)
            | moduleNamespace ns == namespace (module_ t) = updateImports offset as t
            | otherwise = parseFileFailure offset $ "Imported module name does not match, expected " ++ intercalate "." ns

    checkExposed a = do
        let namespaceImported = namespace . importedModule
        Module{..} <- gets module_
        mapM_ (checkSymbols namespace) exposedSymbols
        let moduleNamespaces = namespace : map namespaceImported imports
        mapM_ (checkModules moduleNamespaces) exposedModules
        mapM_ (checkDiffs moduleNamespaces) exposedDiffs
        modify updateVisible
        return a
      where
        checkSymbols ns x = do
            s <- findSymbol $ copyNote (ScopedNameF [ns] Nothing x) x
            case s of
               Just (Variable _ _ t _ _ _) | not (isConst t)
                       -> registerSymbolError x "global, mutable state can't be exposed outside a module"
               Nothing -> registerSymbolError x "undefined symbol"
               _       -> return ()
        checkModules ns x
            | x `elem` ns = return ()
            | otherwise = registerFancyError 0 $ "Missing import for re-exposed module " ++ toString (unmangleModuleNamespace x)

        checkDiffs ns (x, y)
            | y `elem` ns = checkModules ns x
            | otherwise = registerFancyError 0 $ "Missing import for module used in re-exposed module difference " ++ toString (unmangleModuleNamespace y)


    checkRedundantImports (ModuleDecl _ _ a) = checkRedundantImports a
    checkRedundantImports x@(Seq a) = do
        Module{..} <- gets module_
        return $ copyNote (SeqF $ foldr redundantWarning (copyNote (SeqF []) x) imports : a) x
      where
        redundantWarning (Import _ _ offset Redundant) = warning offset RedundantImport "The import is redundant"
        redundantWarning _ = id
    checkRedundantImports _ = undefined

    declarations = parseImports >> many decl
      where
        decl = choice
            [ try $ globalVariable <* semi
            , staticIf decl
            , commonDecl
            , function FreeFunction
            , functionTemplate FreeFunction
            ] <* hidden (skipMany semi)

    commonDecl = choice
        [ try exportType
        , try extern
        , alias
        , enum
        , struct
        , union
        , staticAssert
        , try structTemplate
        , try unionTemplate
        , try aliasTemplate
        , try classTemplate
        , class_
        ] <* hidden (skipMany semi)

    identifier = noted $ Identifier <$> name

    alias = symbol $ alias' []

    aliasTemplate = template alias'

    alias' params = (do
        ident <- keyword "using" *> identifier <* equal
        Alias <$> getScopeQualifier <*> unscopedWith params typ <*> pure ident <* semi) <?> "type alias"

    exportType = symbol (ExportType <$> optionalAttributes [strAttr Rename] <* keyword "export" <*> (try templateSpecifier <|> typ) <* semi <?> "export declaration")

    extern = Extern <$> optionalAttributes [strAttr Rename] <* keyword "extern" <*> (try templateSpecifier <|> typeSpecifier) <* semi <?> "extern declaration"

    enum = (do
        ident <- keyword "enum" *> identifier
        symbol (Enum <$> getScopeQualifier <*> pure ident <* colon <*> typ  <*> scoped ident (braces enumConsts))) <?> "enum"
      where
        enumConsts = do
            scope <- getScopeQualifier
            Seq <$> enumConsts' scope
        enumConsts' scope = do
            x <- symbolWithDocs (EnumConstant scope <$> identifier <*> optional (equal *> expression) <?> "enum constant")
            xs <- unscopedWith [x] (comma >> enumConsts' scope) <|> pure []
            return (x : xs)

    struct = symbol $ struct' []

    structTemplate = template struct'

    struct' params =
        Struct <$ keyword "struct" <*> getScopeQualifier <*> identifier <*> quasiScopedWith params (braces (memberVariable `sepEndBy` semi)) <?> "struct"

    union = symbol $ union' []

    unionTemplate = template union'

    union' params = Union <$ keyword "union" <*> getScopeQualifier <*> identifier <*> quasiScopedWith params (braces (memberVariable `sepEndBy` semi)) <?> "union"

    lookAheadSkippingMemberBody = local (\x -> x {skipMemberBody = True}) . optional . lookAhead . try

    lookAheadSkippingClassBody = local (\x -> x {skipClassBody = True}) . optional . lookAhead . try

    isVariable Variable{} = True
    isVariable _ = False

    defaultInitialization = DefaultInitialization <$ keyword "default" <* equal <*> expression <* semi

    class_ = do
        self <- lookAheadSkippingMemberBody $ class' [] []
        let symbols = case self of
                        Just x@(Class _ _ (Seq members)) -> x : filter (\m -> isTemplate m || isType m || isVariable m) members
                        _ -> []
        symbol $ class' symbols []

    classTemplate = do
        decl <- lookAheadSkippingClassBody $ template $ class' []
        self <- lookAheadSkippingMemberBody $ template $ class' $ maybeToList decl
        let symbols = case self of
                        Just x@(Template _ (Class _ _ (Seq members))) -> x : filter (\m -> isTemplate m || isType m || isVariable m) members
                        _ -> []
        template $ class' symbols

    class' symbols params = (do
        scope <- getScopeQualifier
        ident <- keyword "class" *> identifier
        skipBody <- asks skipClassBody
        unscopedWith symbols $
            scopedWith params ident $
                Class scope ident <$> if skipBody
                    then Seq [] <$ L.skipBraces
                    else braces $ many memberDecl) <?> "class"
      where
        memberDecl = choice
            [ Private <$ keyword "private" <* colon
            , Public <$ keyword "public" <* colon
            , staticIf memberDecl
            , commonDecl
            , defaultInitialization
            , try $ classVariable <* semi
            , function MemberFunction
            , functionTemplate MemberFunction
            ]

    templateParameters = (do
        scope <- getScopeQualifier
        params <- keyword "template" *> angles (paramList scope 0)
        let names = map getName params :: [String]
        let duplicateNames = names \\ nub names
        if null duplicateNames
            then return params
            else fail $ "Redefinition of template parameter(s) " ++ show duplicateNames) <?> "template"
      where
        paramList scope i = do
            x <- notedWithDocs $ typenameParam <|> templateParam <|> nonTypeParam
            xs <- unscopedWith [x] (comma >> paramList scope (i + 1)) <|> pure []
            return (x : xs)
          where
            typenameParam = TemplateParam scope <$> typename <*> name <*> pure i <*> optional (equal *> typ)
            nonTypeParam  = TemplateParam scope <$> typ <*> name <*> pure i <*> optional (equal *> expressionTemplate)
            templateParam = TemplateParam scope <$> typeTemplateSig <*> name <*> pure i <*> optional (equal *> templateSpecifier)

            typeTemplateSig = Template <$ keyword "template" <*> angles (paramKind `MP.sepBy` comma) <*> typename
            paramKind = typename <|> typ

    template def = symbol $ do
        params <- templateParameters
        Template params <$> noted (def params)

    funcParams = parens (symbol param `sepBy` comma)
      where
        param = FuncParam <$> getScopeQualifier <*> paramAttr <*> typ <*> identifier <?> "function parameter declaration"
        paramAttr = optionalAttributes
            [ flagAttr EndTransaction
            ]

    funcTypeAttr =
        [ flagAttr Async
        , flagAttr NoBackPressure
        , flagAttr Pipelined
        , flagAttr Unordered
        , intAttr Latency
        , intAttr MaxThreads
        , intAttr ThreadRate
        ]

    funcAttr = optionalAttributes (funcTypeAttr ++
        [ flagAttr Reset
        , flagAttr Pure
        ])

    funcModifier = choice
        [ modifier Inline
        , modifier NoInline
        ]
      where
        modifier x = FunctionModifier x <$ keyword (show x)

    functionDef kind modifier self templateParams = do
        scope <- getScopeQualifier
        unscopedWith templateParams $ do
            attr <- funcAttr
            modi <- optional modifier
            type_ <- typ
            ident <- identifier
            skipBody <- asks skipMemberBody
            unscopedWith (maybeToList self) $
                scoped ident $ do
                    params <- funcParams
                    decl <- if isNothing modi && kind == MemberFunction
                                then optional semi
                                else return Nothing
                    if isJust decl
                        then return $ FunctionDecl scope attr type_ ident params
                        else Function scope kind attr modi type_ ident params <$> if skipBody
                            then Seq [] <$ L.skipBraces
                            else functionScoped $ braces $ many statementOrDecl

    function kind = symbol $ functionDef kind funcModifier Nothing []

    functionTemplate kind = do
        self <- lookAheadSkippingMemberBody $ template $ functionDef kind funcModifier Nothing
        template $ functionDef kind funcModifier self

    decltype = noted $ Decltype <$ keyword "decltype" <*> getScopeQualifier <*> parens expression

    lambdaIdent = noted $ Identifier <$> makeSymbolName "?lambda" lambdaIndex (\x y -> x { lambdaIndex = y })

    lambda = noted $ do
        scope <- getScopeQualifier
        ident <- lambdaIdent
        auto <- noted $ pure Auto
        scoped ident $ do
            captures@(Seq cs) <- brackets (noted capture `sepBy` comma)
            params <- funcParams <|> pure (Seq [])
            ret <- returnType <|> pure auto
            functionScopedWith cs (Lambda scope ident captures params ret <$> braces (many statementOrDecl) <?> "lambda expression")
      where
        returnType = L.symbol "->" *> typ
        capture = (Capture . functionScope <$> getScopeQualifier) <*> identifier

    uninitVar flags = noted (Variable <$> getScopeQualifier <*> emptySeq <*> typ <*> identifier <*> pure Nothing <*> pure flags)

    uninitConst = symbol $ Variable <$> getScopeQualifier <*> emptySeq <* keyword "const" <*> noted (Const <$> typ) <*> identifier <*> pure Nothing <*> pure DeclareUninitConst

    variable flags attr = symbol $ do
        skipBody <- asks skipMemberBody
        Variable <$> getScopeQualifier <*> attr <*> typ <*> identifier <*> (if skipBody
               then try (optional (equal *> expression <* lookAhead semi)) <|> Nothing <$ skipInit
               else optional (equal *> expression)) <*> pure flags
      where
        skipInit = equal <* takeWhileP Nothing (/= ';')

    globalVariable = variable DeclareGlobal emptySeq
    classVariable = variable DeclareClass $ optionalAttributes [strAttr Rename]
    localVariable = variable DeclareLocal emptySeq
    staticVariable = keyword "static" *> variable DeclareStatic emptySeq
    memberVariable = uninitVar DeclareMember

    nestedScope p = NestedScope <$> getScopeQualifier <*> getQuasiScopeIdentifier <*> quasiScoped (braces (many p))

    localNestedScope p = NestedScope <$> getScopeQualifier <*> getQuasiScopeIdentifier <*> localScoped (braces (many p))

    statementOrDecl = noted $ choice
        [ commonDecl
        , try $ functionTemplate FreeFunction
        , statement
        , try $ keyword "static" *> defaultInitialization
        , staticVariable <* semi
        , try $ localVariable <* semi
        , function FreeFunction
        ] <* hidden (skipMany semi)

    statement = noted $ choice
        [ hidden $ semi $> Seq []
        , barrier <* semi
        , reorder
        , atomic
        , annotatedStmt
        , returnStmt
        , incDecStmt <* semi
        , assignStmt <* semi
        , hidden $ localNestedScope statementOrDecl
        , try $ expression <* semi
        , controlFlow
        ]
      where
        barrier = Barrier <$ keyword "barrier"
        reorder = Reorder <$ keyword "reorder" <*> statement
        controlFlow = choice
            [ loop
            , staticFor
            , staticIf statementOrDecl
            , ifStmt
            , switch
            ] <?> "control flow statement"

    memoryAttr = attributes
        [ flagAttr Initialize
        , flagAttr Memory
        , flagAttr NonReplicated
        , flagAttr QuadPort
        , identAttr Ecc
        ]

    typename = Typename <$ keyword "typename"

    typ = typ' True

    typ' allowUnresolved = noted (choice
        [ voidType
        , constType <*> noted type'
        , type'
        ] <?> "type")
      where
        voidType = Void <$ keyword "void"
        constType = Const <$ keyword "const"
        stringType = String <$ keyword "string"
        autoType = Auto <$ keyword "auto"
        type' = choice
            [ autoType
            , Array <$> try memoryAttr <*> type''' <*> (Seq . pure <$> brackets expression)
            , functionType
            , type''
            ]
        type'' = do
            t <- noted type'''
            b <- many (brackets expression)
            return $ case b of
                Seq [] -> t
                x      -> Array (Seq []) t x
        type''' = choice
            [ Boolean <$ keyword "bool"
            , Float <$ keyword "float32"
            , try (Integer <$ string "int"  <*> intSuffix)
            , try (Unsigned <$ string "uint" <*> intSuffix)
            , ParamInt  <$ keyword "int"  <*> angles expressionTemplate
            , ParamUint <$ keyword "uint" <*> angles expressionTemplate
            , typeSpecifier' allowUnresolved
            , keyword "typename" *> typeSpecifier' True
            , decltype
            , stringType
            ]
        functionType = FunctionType <$> attr <*> quasiScoped (parens (param `sepBy` comma)) <* L.symbol "->" <*> returnType <?> "function type"
          where
            param = FunctionTypeParam <$> paramAttr <*> paramType <*> optional (symbol identifier)
            paramType = noted $ constType <*> type'' <|> noted autoType <|> noted stringType <|> type''
            attr = optionalAttributes funcTypeAttr
            paramAttr = optionalAttributes [ flagAttr EndTransaction ]
            returnType = noted voidType <|> noted autoType <|> type''

    atomic = Annotated <$ keyword "atomic" <*> atomicAttr <*> statement <?> "atomic statement"
      where
        atomicAttr = pure $ Seq [FlagAttr Atomic]

    stmtAttr = attributes
        [ intAttr Schedule
        ]

    annotatedStmt = Annotated <$> try stmtAttr <*> statement

    returnStmt = Return <$ keyword "return" <*> optional expression <* semi

    staticAssert = choice
        [ StaticAssertLegacy <$ keyword "static_assert" <*> parens expression <* semi <?> "static assertion"
        , StaticAssert <$ keyword2 "static" "assert" <*> parens expression <* semi <?> "static assertion"
        ]

    open k = keyword k <* lparen

    close = rparen

    loop = do
        attr <- optionalAttributes [ flagAttr Unordered, flagAttr ReorderByLooping, intAttr FifoDepth ]
        rangeFor attr <|> doWhile attr
      where
        rangeFor attr = localScoped $ RangeFor attr <$ open "for" <*> uninitConst <* colon <*> expression <* close <*> statement

        doWhile attr = do
            stmt <- keyword "do" *> statement
            cond <- keyword "while" *> parens expression
            return $ DoWhile attr cond stmt

    staticFor = localScoped $ StaticFor <$ try (keyword "static" <* keyword "for") <* lparen <*> uninitConst <* colon <*> expression <* rparen <*> statement

    staticIf p = StaticIf <$ try (keyword "static" <* keyword "if") <*> parens expression <*> p' <*> optional (keyword "else" *> p')
      where
        p' = nestedScope p <|> p

    ifStmt = If <$ keyword "if" <*> parens expression <*> statement <*> optional (keyword "else" *> statement)

    switch = Switch <$ keyword "switch" <*> parens expression <*> braces (many block)
      where
        block = choice
            [ SwitchCase <$ keyword "case" <*> expression <* colon <*> localScoped (many statementOrDecl) <* keyword "break" <* semi
            , SwitchDefault <$ keyword "default" <* colon <*> localScoped (many statementOrDecl) <* keyword "break" <* semi
            , staticIf block
            ]

    incDecStmt = choice
        [ op "--" Sub
        , op "++" Add
        ]
      where
        op s o = do
            lval <- (operator s *> lvalue) <|> try (lvalue <* operator s)
            return $ Assign lval $ Binary o lval $ IntLiteral Nothing 1

    assignStmt = noted $ choice
        [ try assign
        , assign' "+="  Add
        , assign' "-="  Sub
        , assign' "*="  Mul
        , assign' "/="  Div
        , assign' "%="  Mod
        , assign' "&="  BitwiseAnd
        , assign' "|="  BitwiseOr
        , assign' "^="  BitwiseXor
        , assign' "&&=" And
        , assign' "||=" Or
        , assign' "^^=" Xor
        ]
      where
        assign = Assign <$> lvalue <* (equal <?> "assignment operator") <*> expression
        assign' s o = try $ do
            lval <- lvalue <* (L.symbol s <?> "assignment operator")
            let rval = desugar rvalue lval
            x <- expression
            return $ Assign lval (copyNote (BinaryF o rval x) lval)
        rvalue (ArrayAccessLValueF a b) = ArrayAccessF a b
        rvalue e = e

    qualifiedIdentifier = fst <$> qualifiedIdentifierAndSymbol <?> "scoped identifier"

    qualifiedIdentifierAndSymbol = do
        (s, sym) <- scopedNameAndSymbol
        return (copyNote (QualifiedIdentifierF s) s, sym)

    templateQualifier = try (L.symbol "template" <* notFollowedBy langle) <?> "`template` qualifier"

    scopedNameAndSymbol = do
        scope <- getScopeQualifier
        prefixedScopedNameAndSymbol scope Nothing Nothing False

    prefixedScopedNameAndSymbol scope prefix resolvedPrefix isQualified = do
        forceTemplate <- isJust <$> optional templateQualifier
        ident <- noted identifier
        let s = copyNote (ScopedNameF scope prefix ident) ident
        sym <- findSymbol $ copyNote (ScopedNameF scope resolvedPrefix ident) ident

        (prefix', resolvedPrefix', isQualified') <- case sym of
            Just e@(TemplateParam n a b c _) -> do
                let p = copyNote (TypeParamF n a b c) s
                if isTemplate a
                    then do
                        i <- templateInstance p e
                        return (i, i, True)
                    else
                        return (p, p, True)
            Just e@(Alias _ (TypeIdentifier _ t) _) ->
                return (qualifyScope e s, t, True)
            Just e
                | forceTemplate || isTemplate e -> do
                    i <- templateInstance s e
                    return (qualifyScope e i, i, True)
                | otherwise ->
                    return (qualifyScope e s, s, True)
            Nothing
                | forceTemplate -> do
                    i <- templateInstance s s
                    return (i, i, True)
                | otherwise ->
                    return (s, s, isQualified)

        sep <- isJust <$> optional (L.symbol "::")
        if sep then prefixedScopedNameAndSymbol scope (Just prefix') (Just resolvedPrefix') isQualified'
               else return (prefix', sym)
      where
        qualifyScope e
            | not isQualified = makeQualified e
        qualifyScope _        = id

        templateInstance s t = do
            a <- optional templateArgs
            return $ case a of
                Just args        -> copyNote (TemplateInstanceF s args) s
                _ | isFunction t -> copyNote (TemplateInstanceF s $ Seq []) s
                  | otherwise    -> s

    templateArgs = angles ((try templateSpecifier <|> expressionTemplate) `sepBy` comma) <?> "template arguments"

    isInstance TemplateInstance{} = True
    isInstance _ = False

    templateSpecifier = noted $ do
        scope <- getScopeQualifier
        (s, sym) <- scopedNameAndSymbol
        case sym of
            Just t | isTemplate t && not (isInstance s)
                -> return $ TypeIdentifier scope s
            _   -> unexpectedSymbol s "template specifier"

    typeSpecifier = typeSpecifier' True

    typeSpecifier' allowUnresolved = noted $ do
        scope <- getScopeQualifier
        (s, sym) <- scopedNameAndSymbol
        case sym of
            Just t@TemplateParam{}
                | isType t && not (isTemplate t)
                                     -> return s
            Just t
                | isTemplate t && not (isInstance s)
                                     -> symbolError s "template arguments missing"
                | isType t           -> return $ TypeIdentifier scope s
                | otherwise          -> unexpectedSymbol s "type specifier"
            Nothing
                | allowUnresolved    -> return $ TypeIdentifier scope s
                | otherwise          -> unexpectedSymbol s "type specifier"

    functionSpecifier = noted (methodSpecifier <|> FunctionSpecifier Nothing <$> freeFunctionSpecifier)

    methodSpecifier = do
        scope <- getScopeQualifier
        obj <- try (lvalue <* dot)
        t <- resolveTypeName obj
        FunctionSpecifier (Just obj) <$> noted (QualifiedIdentifier . fst <$> prefixedScopedNameAndSymbol scope unresolved t True)
      where
        resolveTypeName (ArrayAccessLValue x _) = resolveTypeName x
        resolveTypeName (NamedValue x)          = resolveTypeName x
        resolveTypeName (QualifiedIdentifier x) = resolveTypeName x
        resolveTypeName (MemberAccess x y) = do
            scope <- getScopeQualifier
            t <- resolveTypeName x
            resolveTypeName $ ScopedName scope t y
        resolveTypeName s@(ScopedName _ _ x) = do
            sym <- findSymbol s
            case sym of
               Nothing -> do
                   sym' <- findSymbol x
                   case sym' of
                       Just (Variable _ _ t _ _ _) -> typeName t
                       _                           -> return unresolved
               Just (Variable _ _ t _ _ _) -> typeName t
               _                           -> return unresolved
        resolveTypeName _ = return unresolved

        typeName (Array _ x _) = typeName x
        typeName (TypeIdentifier _ x) = do
            sym <- findSymbol x
            case sym of
                Just (Alias _ t@(TypeIdentifier _ _) _) -> typeName t
                Just _                                  -> return $ Just x
                Nothing                                 -> return unresolved
        typeName _ = return unresolved

        unresolved = Just $ ScopedName [] Nothing (Identifier unresolvedName)

    methodInstanceSpecifier = do
        m@(FunctionSpecifier _ (QualifiedIdentifier a)) <- noted methodSpecifier
        case a of
            TemplateInstance{} -> return m
            _ -> symbolError m "undefined template"

    freeFunctionSpecifier = noted $ do
        (s, sym) <- qualifiedIdentifierAndSymbol
        case sym of
            Just (TemplateParam n a@FunctionType{} b c _)
                                    -> return $ TypeParam n a b c
            Just TemplateParam{}
                                    -> symbolError s "illegal use of this template parameter as a function"
            Just (Variable _ _ Const{} _ (Just x@NamedValue{}) _)
                                    -> return x
            _                       -> return s

    namedValue = noted $ do
        (s, sym) <- qualifiedIdentifierAndSymbol
        case sym of
            Just t
                | isType t          -> symbolError t "illegal use of this type as an expression"
            Just (TemplateParam n a@FunctionType{} b c _)
                                    -> return $ NamedValue $ copyNote (TypeParamF n a b c) s
            Just (TemplateParam n a b c _)
                                    -> return $ TypeParam n a b c
            _                       -> return $ NamedValue s

    namedLValue = noted $ do
        (s, sym) <- qualifiedIdentifierAndSymbol
        case sym of
            Just t
                | isType t          -> symbolError s "illegal use of this type as an expression"
            Just (Variable _ _ Const{} _ _ _)
                                    -> symbolError s "illegal use of this constant as an lvalue"
            _                       -> return $ NamedValue s

    lvalue = noted $ do
        l <- namedLValue
        access ArrayAccessLValueF l

    value = noted $ do
        v <- namedValue
        access ArrayAccessF v

    access arrayAccess l = do
        e <- optional $ choice
            [ brackets expression
            , try (dot *> identifier <* notFollowedBy lparen <* notFollowedBy templateArgs)
            ]
        case e of
            Nothing             -> return l
            Just i@Identifier{} -> access arrayAccess $ copyNote (MemberAccessF l i) l
            Just e'             -> access arrayAccess $ copyNote (arrayAccess l e') l

    strLiteral = noted $ StringLiteral <$> stringLiteral

    expression = do
        offset <- getOffset
        e <- expr' False
        if isType e
            then registerFancyError offset "Invalid use of a type as an expression" >> return e
            else return e

    expressionTemplate = expr' True

    expr' t = noted $ if t then exprTemplate else expr
      where
        expr = makeExprParser term ops <?> "expression"
        exprTemplate = makeExprParser term opsTemplate <?> "expression"
        term = noted $ choice
            [ mux
            , lutmul
            , concat'
            , fanout
            , static
            , bitoffset
            , byteoffset
            , try $ typ' False
            , hidden (parens expr)
            , literal
            , hidden cast
            , functionCall
            , methodInstanceSpecifier
            , value
            , try designatedInitializerList
            , initializerList
            , lambda
            ]
          where
            initializerList = InitializerList <$> braces (Seq <$> positionalList 0)
              where
                positionalList i = do
                    x <- noted $ Positional i <$> expression
                    xs <- try (comma >> positionalList (i + 1)) <|> pure []
                    return (x:xs)
            designatedInitializerList = InitializerList <$> braces (designator `sepBy` comma)
            designator = Designator <$ char '.' <*> identifier <* equal <*> expression
            bitoffset = BitOffset <$ open "bitoffsetof" <*> typ <* comma <*> identifier <* close
            byteoffset = ByteOffset <$ open "byteoffsetof" <*> typ <* comma <*> identifier <* close
            lutmul = Binary LutMul <$ open "lutmul" <*> expression <* comma <*> expression <* close
            concat' = Concat <$ keyword "concat" <*> parens (expression `sepBy1` comma)
            fanout = FanOut <$ keyword "fan_out" <*> angles expressionTemplate <*> parens expression
            static = Static <$ keyword "static" <*> parens expression
            mux = Mux <$ open "mux" <*> expression <* comma <*> expression `sepBy1` comma <* close
            functionCall = FunctionCall <$> callModifiers <*> try (functionSpecifier <* lparen) <*> (expression `sepBy` comma) <* rparen <?> "function call"
            cast = Cast <$ keyword "cast" <*> angles typ <*> parens expression
            callModifiers = optionalAttributes
                [ intAttr CallRate
                , intAttr FifoDepth
                , intAttr TransactionSize
                ]
            literal = choice
                [ try $ FloatLiteral <$> float
                , intLiteral
                , BoolLiteral True  <$ keyword "true"
                , BoolLiteral False <$ keyword "false"
                , InterpolatedString <$ char '"' <*> stringSegments <* L.symbol "\""
                ] <?> "literal"
              where
                stringSegments = do
                    str <- manyTill charLiteral $ lookAhead (char '"' <|> try singleBrace)
                    fe <- optional $ between (L.symbol "{") (char '}') formatedExpr
                    xs <- (lookAhead (char '"') >> pure []) <|> stringSegments
                    return $ (str, fe) : xs
                  where
                    charLiteral = char '{' *> MP.charLiteral <|> MP.charLiteral
                    singleBrace = char '{' <* notFollowedBy (char '{')
                formatedExpr = do
                    e <- expression
                    n <- optional equal
                    a <- optional (comma >> expression)
                    f <- optional (colon >> format)
                    return (e, isJust n, a, f)
                format = L.lexeme $ Format <$> choiceViaShow [Bin, Oct, Dec, Hex, HexUpper] <*> optional L.decimal
                  where
                    choiceViaShow :: Show a => [a] -> Parser a
                    choiceViaShow = choice . map (\x -> string (show x) >> pure x)
                intLiteral = L.lexeme $ do
                    x <- integer
                    IntLiteral <$> optional (typeSuffix SignedLiteral <|> typeSuffix UnsignedLiteral) <*> pure x
                  where
                    typeSuffix x = LiteralType x <$ string (show x) <*> L.decimal

        -- Disallow '>', '<', `>>` and '<<' operators in the top level expression
        -- used for template parameters. They can be used within sub-expressions
        -- wrapped in parenthesis (achieved via the `parens expression` term).
        opsTemplate = ops' [] []
        ops = ops'
            [ no_assoc Greater
            , no_assoc Less
            ]
            [ binary ShiftRight
            , binary ShiftLeft
            ]
        ops' relOps shiftOps =
            [ [ prefix Neg
              ]
            , [ prefix Not
              , prefix Invert
              ]
            , [ prefix' BitSizeof
              , prefix' ByteSizeof
              , prefix' Clog2
              ]
            , [ binary Mul
              , binary Div
              , binary Mod
              ]
            , [ binary Add
              , binary Sub
              ]
            , shiftOps
            , [ no_assoc LessEqual
              , no_assoc GreaterEqual
              ]
              ++ relOps
            , [
                no_assoc Equal
              , no_assoc NotEqual
              ]
            , [ binary BitwiseAnd
              ]
            , [ binary BitwiseXor
              ]
            , [ binary BitwiseOr
              ]
            , [ binary And
              ]
            , [ binary Xor
              ]
            , [ binary Or
              ]
            , [ ternary "?" ":" ternaryMux
              ]
            ]
        binary op   = InfixL $ binaryNoted (Binary op <$ operator (show $ pretty op) <?> "binary operator")
        no_assoc op = InfixN $ binaryNoted (Binary op <$ operator (show $ pretty op) <?> "binary operator")
        prefix op   = Prefix $ unaryNoted (Unary op <$ operator (show $ pretty op) <?> "unary prefix operator")
        prefix' op  = Prefix $ unaryNoted (Unary op <$ keyword (show $ pretty op) <?> "unary prefix operator")

        ternary o1 o2 f  = TernR ((f <$ operator o2) <$ operator o1 <?> "ternary operator")
        ternaryMux a b c = Mux a $ Seq [c, b]
