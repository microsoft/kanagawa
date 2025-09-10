{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module ParseTree (
    compile
    ) where

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Foreign
import Foreign.C.String
import Language.Kanagawa.Desugar ((.:))
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.Pattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Options.FFI
import ParseTree.FFI
import ParseTree.Types
import System.FilePath
import Text.Megaparsec (SourcePos(..))
import Language.Kanagawa.Type

compile :: Show e => Options -> String -> [FilePath] -> NotedExp (Typed Src) e -> IO Bool
compile opt@Compile{..} cmdArgs allFiles p =
    withCAString (show backend) $ \ptrBackend ->
    withCAString outputBaseName $ \ptrOutput ->
    withCAString resource_usage $ \ptrRes ->
    withCAString dgml           $ \ptrDgml ->
    withCAString dgml_detailed  $ \ptrDgmlDetailed -> do
        x <- withOptions opt cmdArgs allFiles initCompiler
        if x
            then cataM parseWithLoc p >>= codegen ptrBackend ptrOutput ptrRes ptrDgml ptrDgmlDetailed
            else return False
  where
    symbols = getSymbols'' AllSymbols p
    withName = withCAString . toString

    withQualifiedName = withStringArray . map toString

    outputBaseName = if null output && not (null allFiles) then takeBaseName (last allFiles) else output

    parseWithLoc e@(Note n _) = do
        setSrcLocation $ untype n
        node <- parse e
        when (tIsInt $ typeOf e) $ parseIntegerType (typeOf e) >>= setNodeType node
        when (tIsEnum $ typeOf e) $ parseIntegerType (tEnumBase $ typeOf e) >>= setNodeType node
        return node

    setSrcLocation SrcUnknown = unknownLocation
    setSrcLocation (SrcStack a _) = setSrcLocation a
    setSrcLocation (Src _ begin end _) = withLocation loc setLocation
      where
        loc = Location begin end $ fromJust $ elemIndex (sourceName begin) allFiles

    parse expr = case expr of
        Alias n a b                         -> withQualifiedName n $ withUnmangledName . parseTypedef a b
        Annotated a b                       -> parseAnnotatedStatement a b
        Array a b c                         -> parseArrayType a b c
        ArrayAccess a b                     -> parseAccessArray a b
        ArrayAccessLValue a b               -> parseAccessArrayLValue a b
        Assign a b                          -> parseAssign a b
        Attr a b                            -> parseAttribute (attr a) b
        Auto                                -> undefined
        Barrier                             -> parseBarrier
        Binary op e1 e2                     -> parseBinaryOp (fromEnum op)  e1 e2
        BitOffset{}                         -> undefined
        Boolean                             -> parseBool
        BoolLiteral x                       -> parseBoolLiteral $ fromEnum x
        ByteOffset{}                        -> undefined
        Capture _ _                         -> undefined
        Cast a b                            -> parseCast a b
        Class n a b                         -> parseInstanceType (theType expr) >>= withQualifiedName n . withUnmangledName .: parseClass a b
        Concat a                            -> parseConcat a
        Const a                             -> parseConst a
        Decltype{}                          -> undefined
        DefaultInitialization a             -> parseDefaultInitialization a
        Designator a b                      -> parseDesignator a b
        DoWhile a b c                       -> parseDoWhile a b c
        Enum n a b c                        -> withQualifiedName n $ parseEnum a b c
        EnumConstant _ _ Nothing            -> undefined
        EnumConstant n a (Just b)           -> withQualifiedName n $ parseEnumConstant a b
        EnumValue a b                       -> parseEnumValue a b
        Error{}                             -> undefined
        ExportType a b                      -> parseExportType a b
        Extern a b                          -> parseExtern a b
        FanOut a b                          -> parseFanOut a b
        FlagAttr Async                      -> parseFunctionModifier AsyncFunc
        FlagAttr Atomic                     -> parseIntLiteral (1 :: Int) >>= parseIntAttribute ScheduleAttr
        FlagAttr EndTransaction             -> parseFlagAttribute EndTransactionParam
        FlagAttr Initialize                 -> parseFlagAttribute MemoryInitialize
        FlagAttr Memory                     -> parseFlagAttribute MemoryDefault
        FlagAttr NoBackPressure             -> parseFunctionModifier NoBackPressureFunc
        FlagAttr Pure                       -> parseFunctionModifier PureFunc
        FlagAttr NonReplicated              -> parseFlagAttribute MemoryNorep
        FlagAttr Pipelined                  -> parseFunctionModifier PipelinedFunc
        FlagAttr QuadPort                   -> parseFlagAttribute MemoryQuadPort
        FlagAttr ReorderByLooping           -> parseFunctionModifier ReorderByLoopingFunc
        FlagAttr Reset                      -> parseFunctionModifier ResetFunc
        FlagAttr Unordered                  -> parseFunctionModifier UnorderedFunc
        Float                               -> parseFloatType
        FloatLiteral x                      -> parseFloatLiteral x
        FuncParam n a b c                   -> withQualifiedName n $ parseFunctionParam a b c
        Function _ _ a (Just b) c d e f     -> withUnmangledName $ parseFunction a b c d e f
        Function _ _ a Nothing c d e f      -> withUnmangledName $ parseFunction a nullPtr c d e f
        FunctionCall a b c                  -> parseFunctionCall b c a
        FunctionDecl _ a b c d              -> parseExternalFunction a b c d
        FunctionModifier a                  -> parseFunctionModifier $ modifier a
        FunctionSpecifier a b               -> parseFunctionSpecifier (fromMaybe nullPtr a) b
        FunctionType a b c                  -> parseFunctionType a b c
        FunctionTypeParam a b c             -> parseFunctionTypeParam a b $ fromMaybe nullPtr c
        Identifier x                        -> withName x parseIdentifier
        If a b c                            -> parseIf a b $ fromMaybe nullPtr c
        InitializerList a                   -> parseInitializerList a
        IntAttr a b                         -> parseIntAttribute (intAttr a) b
        Integer n                           -> parseIntType n
        IntLiteral _ x                      -> parseIntLiteral x
        InterpolatedString a                -> parseList parseSegment a >>= parseInterpolatedString
        Lambda{}                            -> undefined
        MemberAccess a b                    -> parseAccessMember a b
        ModuleDecl a _ b                    -> parseNamespace a b
        ModuleDiff _ _                      -> return nullPtr
        ModuleIdentifier a                  -> withName (moduleNamespace a) parseIdentifier
        Mux a b                             -> parseMux a b
        NamedValue a                        -> parseNamedVariable a
        NestedScope _ _ a                   -> parseNestedScope a
        ParamInt{}                          -> undefined
        ParamUint{}                         -> undefined
        Positional _ a                      -> return a
        Private                             -> parseMemberModifier MemberModifierPrivate
        Public                              -> parseMemberModifier MemberModifierPublic
        QualifiedIdentifier a               -> parseScopedIdentifier a
        RangeFor a b c d                    -> parseRangeFor a b c d
        Reference a                         -> parseReference a
        Reorder a                           -> parseReorder a
        Return (Just e)                     -> parseReturnExpr e
        Return Nothing                      -> parseReturn
        Seq a                               -> parseList return a
        ScopedName _ (Just a) b             -> parseAppendList a b
        ScopedName _ Nothing a              -> parseBaseList a
        Static a                            -> parseStatic a
        StaticAssert _                      -> undefined
        StaticAssertLegacy a                -> parseStaticAssert a
        StaticFor a b c                     -> parseUnrolledFor a b c
        StaticIf{}                          -> undefined
        String                              -> parseStringType
        StringLiteral s                     -> parseString s
        Struct n a b                        -> withQualifiedName n $ withUnmangledName . parseStruct a b
        Switch a b                          -> parseSwitch a b
        SwitchCase a b                      -> parseSwitchBlock a b
        SwitchDefault a                     -> parseSwitchBlock nullPtr a
        Template{}                          -> undefined
        TemplateInstance{}                  -> undefined
        TemplateParam{}                     -> undefined
        This n                              -> withQualifiedName n parseThis
        TypeIdentifier n a                  -> withQualifiedName n $ parseNamedType a
        Typename                            -> undefined
        TypeParam{}                         -> undefined
        Unary BitSizeof e                   -> parseSizeOf SizeofTypeBit e
        Unary ByteSizeof e                  -> parseSizeOf SizeofTypeByte e
        Unary Clog2 _                       -> undefined
        Unary op e                          -> parseUnaryOp (fromEnum op) e
        Union n a b                         -> withQualifiedName n $ withUnmangledName . parseUnion a b
        UnrolledFor a b c                   -> parseUnrolledFor a b c
        Unsigned n                          -> parseUintType n
        Variable n a b c d x                -> withQualifiedName n $ parseDeclare a b c (fromMaybe nullPtr d) (declareFlag x)
        Void                                -> parseVoid
        Warning _ _ a                       -> return a
      where
        withUnmangledName
            | tIsInstance $ theType expr = withCAString (show $ unmangleInstanceName symbols $ theType expr)
            | tIsInstance $ typeOf expr  = withCAString (show $ unmangleInstanceName symbols $ typeOf expr)
            | otherwise                  = ($ nullPtr)

    declareFlag = \case
        DeclareGlobal       -> DeclareFlagGlobal
        DeclareClass        -> DeclareFlagClass
        DeclareLocal        -> DeclareFlagLocal
        DeclareStatic       -> DeclareFlagStatic
        DeclareMember       -> DeclareFlagMember
        DeclareUninitConst  -> DeclareFlagUninitConst

    attr = \case
        Ecc                 -> MemoryEcc
        Rename              -> NameAttr
        _                   -> undefined

    intAttr = \case
        CallRate            -> CallRateAttr
        FifoDepth           -> FifoDepthAttr
        Latency             -> LatencyAttr
        MaxThreads          -> MaxThreadsAttr
        Schedule            -> ScheduleAttr
        ThreadRate          -> ThreadRateAttr
        TransactionSize     -> TransactionSizeAttr
        _                   -> undefined

    modifier = \case
        Inline              -> InlineFunc
        NoInline            -> NoInlineFunc

    parseList f a = do
        root <- parseBaseList nullPtr
        foldlM (\x y -> f y >>= parseAppendList x) root a

    parseString s = withCAString s parseStringLiteral

    parseIntLiteral x = withCAString (show x) parseDecimalLiteral

    parseSegment (s, e) = parseString s >>= (parseExpression e >>=) . parseInterpolatedStringSegment
      where
        parseExpression Nothing = return nullPtr
        parseExpression (Just (a, _, b, c)) = parseInterpolationExpression a
                (fromMaybe nullPtr b)
                (maybe FormatSpecifierNone (specifier . formatSpecifier) c)
                (maybe 0 (fromMaybe 0 . formatPrecision) c)
          where
            specifier = \case
                Bin      -> FormatSpecifierBin
                Oct      -> FormatSpecifierOct
                Dec      -> FormatSpecifierDec
                Hex      -> FormatSpecifierHex
                HexUpper -> FormatSpecifierHexUpper

    parseIntegerType t
        | tIsSigned t   = parseIntType $ fromJust $ tWidth t
        | tIsUnsigned t = parseUintType $ fromJust $ tWidth t
        | otherwise     = undefined

    parseInstanceType t
        | tIsInstance t = do
            template <- qualifiedName (tInstanceTemplate t)
            args <- templateArgs (tInstanceArgs t)
            parseTemplateInstance template args
        | otherwise = return nullPtr
      where
        qualifiedName x = withQualifiedName x parseQualifiedName
        templateArgs = parseList namedArg
          where
            namedArg (x, a) = withName x $ (arg a >>=) . parseTemplateArg
            arg (TTypeArg x)
                | tIsStruct x || tIsEnum x || tIsUnion x = qualifiedName $ tQualifiedName x
                | tIsInt x                               = parseIntegerType x
            arg (TIntArg x)                              = parseIntLiteral x
            arg (TStrArg x)                              = parseString x
            arg _                                        = return nullPtr

compile _ _ _ _ = undefined
