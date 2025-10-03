{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Frontend
    ( frontend
    , removeRedundantNameScope
    ) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Text.EditDistance
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Function
import Language.Kanagawa.Internal
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Language.Kanagawa.Template
import Language.Kanagawa.Type
import Language.Kanagawa.Type.Inference
import Language.Kanagawa.TypeCheck

frontend :: (Semigroup n, Ord (Token s), Ord s, Ord e, Eq n) => Int -> Int -> Int -> [NotedExp n (ParseError s e)] -> [NotedExp (Typed n) (ParseError s e)]
frontend frontendPasses templatePasses templateIterations = desugarPasses frontendPasses templatePasses templateIterations . map noteType

desugarPasses :: (Semigroup n, Ord (Token s), Ord s, Ord e, Eq n) => Int -> Int -> Int -> [NotedExp (Typed n) (ParseError s e)] -> [NotedExp (Typed n) (ParseError s e)]
desugarPasses frontendPasses templatePasses templateIterations program = fst $ foldr ($) (program, False) $ drop (length passes - frontendPasses) passes
  where
    withSymbols fn p = fn (getSymbols $ fst p) p

    pass fn (p, hasErrors)
        | hasErrors = (p, hasErrors)
        | otherwise = (fn p, hasErrors)

    passPar = pass . parallel

    checkPar fn (p, hasErrors)
        | hasErrors = (p, hasErrors)
        | otherwise = (p', hasErrorSymbol $ getSymbols p')
      where
        p' = parallel fn p

    passes =
        [ withSymbols $
            \symbols -> passPar $ desugar' $ validateProgram symbols
        ,               passPar $ desugar interpolatedStringPost
        ,               passPar $ deduceAuto
        , withSymbols $ passPar . postDesugar
        ,               passPar $ desugar removeTemplate
        , withSymbols $ passPar . validateExtern
        , withSymbols $ pass . validateExportableClasses
        , withSymbols $ passPar . unresolvedTemplates
        ,               passPar $ unresolvedFunctionTemplateArgs
        ,               passPar $ cata typeErrors
        , withSymbols $ undefinedSymbols
        ,               instantiateTemplates templatePasses templateIterations
        ,               pass $ trimExternClasses
        ,               pass $ inferType
        ,               passPar $ intrinsics
        , withSymbols $ passPar . deduceTemplateArgs
        ,               passPar $ desugar $ captureThis . reifyEnums
        , withSymbols $ passPar . desugar' . higherOrderFunctions
        , withSymbols $
            \symbols -> checkPar $ desugar $ validateTypedLiterals
                                           . interpolatedStringPre
                                           . freeNonInlineFunctions
                                           . localMemberFunctions
                                           . redundantNameScope symbols
        ]

undefinedSymbols
    :: SymbolMap (Typed n) (ParseError s e)
    -> ([NotedExp (Typed n) (ParseError s e)], Bool)
    -> ([NotedExp (Typed n) (ParseError s e)], Bool)
undefinedSymbols _ (p, True) = (p, True)
undefinedSymbols symbols (p, _) = (p', or errs)
  where
    (p', errs) = unzip $ parallel (para undefinedSymbol) p

    undefinedSymbol e _
        | isAbbreviatedTemplate e = (e, False)
    undefinedSymbol (Template _ b) x@(Note _ (TemplateF a' _))
        | (Note n (TemplateF a _)) <- fmap fst x = (NotedExp n (TemplateF a b), any snd a')
    undefinedSymbol (TemplateInstance a _) x@(Note _ (TemplateInstanceF _ b'))
        | (Note n (TemplateInstanceF _ b)) <- fmap fst x = (NotedExp n (TemplateInstanceF a b), snd b')
    undefinedSymbol e x@(Note n TypeIdentifierF{})
        | tIsUndefined (typeOf x) && not (hasErrors x) = err n $ errMsg e
    undefinedSymbol e x@(Note n ScopedNameF{})
        | tIsUndefined (typeOf x) && not (hasErrors x) = err n $ errMsg e
    undefinedSymbol e x@(Note n QualifiedIdentifierF{})
        | tIsUndefined (typeOf x) = err n $ errMsg e
    undefinedSymbol _ x@(Note n (MemberAccessF a b))
        | tIsUndefined (typeOf x) && typeResolved (fst a) = (NotedExp n (MemberAccessF (fst a) (renote memberErr $ fst b)), True)
      where
        memberErr e = desugarError $ "Undefined member `" ++ getName e ++ "` of type `" ++ showPretty (typeOf $ fst a) ++ "`"
    undefinedSymbol e (Note _ (FunctionSpecifierF (Just a) _))
        | not $ typeResolved $ fst a = (e, False)
    undefinedSymbol _ x = (Fix $ fmap fst x, hasErrors x)

    hasErrors = any snd

    err n msg = (NotedExp n $ desugarError msg, True)

    hintMsg []  = ""
    hintMsg [x] = "\nPerhaps you meant: " ++ x
    hintMsg xs  = "\nPerhaps you meant one of these:\n\t" ++ intercalate "\n\t" xs

    errMsg e
        | TypeIdentifier{} <- e =
            "Type name not in scope: `" ++ symbolName undefinedName ++ "`" ++ hintMsg (hints tIsType)
        | ScopedName{} <- e =
            "Type name not in scope: `" ++ symbolName undefinedName ++ "`" ++ hintMsg (hints tIsType)
        | QualifiedIdentifier{} <- e =
            "Symbol name not in scope: `" ++ symbolName undefinedName ++ "`" ++ hintMsg (hints nonType)
      where
        undefinedName = getQualifiedName e
        nonType t = tIsTemplate t || (tResolved t && not (tIsType t))
        symbolName = toString . last
        moduleDefined x = '@' == head (toString $ head x) && 2 == length x
        inScope x = let qualifier = init x in
            getQualifier e `isSuffixOf` qualifier &&
            take (length qualifier - length (getQualifier e)) qualifier `isPrefixOf` getScope e

        hints cond
            = map (hint . fst)
            $ take 3
            $ sortOn snd
            $ filter similar
            $ map (editDistance . fst)
            $ filter (cond . typeOf . snd)
            $ HashMap.toList symbols
          where
            hint name
                | moduleDefined name && not (inScope name) =
                    "`" ++ symbolName name ++ "` defined in " ++ toString (unmangleModuleNamespace $ head name)
                | otherwise =
                    "`" ++ symbolName name ++ "`"
            similar (name, distance) =
                (distance <= 1 && moduleDefined name) ||
                (distance <= threshold && inScope name)
            threshold = min 3 (length (symbolName undefinedName) `div` 2)
            editDistance x = (x, restrictedDamerauLevenshteinDistance defaultEditCosts (symbolName undefinedName) (symbolName x))

    errMsg _ = undefined

removeRedundantNameScope :: NotedExp n e -> NotedExp n e
removeRedundantNameScope p = let symbols = getSymbols $ pure p in desugar (redundantNameScope symbols) p

redundantNameScope :: SymbolMap n e -> DesugarAlgebra n e
redundantNameScope symbols e@(QualifiedIdentifierF s@(ScopedName a (Just _) c)) = case lookupSymbol unscopedName symbols of
    Just x
        | getQualifier x == getQualifier s -> QualifiedIdentifierF unscopedName
    _                                      -> e
  where
    unscopedName = copyNote (ScopedNameF a Nothing c) s
redundantNameScope _ e = e

typeErrors :: Algebra (NotedExpF (Typed n) (ParseError s e)) (NotedExp (Typed n) (ParseError s e))
typeErrors e
    | tIsError (typeOf e) && null (errors e') = errorMsg (showPretty $ typeOf e) e'
    | otherwise                               = e'
  where
    e' = Fix e

postDesugar :: (Eq (Token s), Eq e) => SymbolMap (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
postDesugar symbols = desugar
    ( resolveNames symbols
    . staticAssert
    . unresolved
    . returnVoid
    . thisReference symbols
    )

captureThis :: DesugarAlgebra n e
captureThis e@ClassF{} = e { body = desugarShallow captureThis' $ body e }
  where
    captureThis' l@LambdaF{..} = Just $ l { lambdaCaptures = cons this lambdaCaptures }
      where
        this = copyNote (ThisF declScopeName) lambdaCaptures
    captureThis' ClassF{}      = Nothing
    captureThis' e'            = Just e'
captureThis e = e

reifyEnums :: DesugarAlgebra n e
reifyEnums e@EnumF{..} = e { body = renote enumConsts body }
  where
    enumConsts (SeqF xs) = SeqF (reifyValues (IntLiteralF Nothing 0) xs)
    enumConsts _ = undefined

    reifyValues _ [] = []
    reifyValues i ((NotedExp n e'):xs)
        | isNothing $ constValue e' = NotedExp n e' {constValue = Just (NotedExp n i)} : reifyValues (increment $ NotedExp n i) xs
        | otherwise                 = NotedExp n e'                                    : reifyValues (increment $ fromJust $ constValue e') xs
    reifyValues _ _ = undefined

    increment x = BinaryF Add x (copyNote (IntLiteralF Nothing 1) x)
reifyEnums e = e

validateTypedLiterals :: DesugarAlgebra n (ParseError s e)
validateTypedLiterals (IntLiteralF (Just t) x)
    | literalWidth t == 0 = desugarError $ "Invalid literal type specifier `" ++ showPretty t ++ "`"
    | outOfRange t        = desugarError $ "Literal out of range for `" ++ showPretty t ++ "`"
  where
    outOfRange (LiteralType SignedLiteral w)   = x /= truncateInt w x
    outOfRange (LiteralType UnsignedLiteral w) = x /= truncateUInt w x
validateTypedLiterals e = e

localMemberFunctions :: DesugarAlgebra n (ParseError s e)
localMemberFunctions e@FunctionF{..}
    | funcKind == MemberFunction = e { body = desugar makeMember body }
  where
    makeMember x@FunctionF{} = x { funcKind = MemberFunction }
    makeMember x = x
localMemberFunctions e = e

freeNonInlineFunctions :: DesugarAlgebra n (ParseError s e)
freeNonInlineFunctions e@(FunctionF _ FreeFunction _ m _ _ _ _)
    | (Just (FunctionModifier Inline)) <- m = e
    | otherwise = desugarError "Free functions must be declared `inline`"
freeNonInlineFunctions e = e

interpolatedStringPre :: DesugarAlgebra n (ParseError s e)
interpolatedStringPre (InterpolatedStringF [(s, Nothing)]) = StringLiteralF s
interpolatedStringPre (InterpolatedStringF segments) = InterpolatedStringF $ map update segments
  where
    update (s, Just (a, True, b, f)) = (s ++ showPretty a ++ " = ", Just (a, False, b, f))
    update x = x
interpolatedStringPre e = e

interpolatedStringPost :: DesugarAlgebra (Typed n) (ParseError s e)
interpolatedStringPost (InterpolatedStringF segments) = InterpolatedStringF $ map update segments
  where
    update (s, Just (a, b, c, f)) = (s, Just (validateFormat a f, b, validateAlignment <$> c, updatePrecision <$> f))
      where
        updatePrecision x@Format{..}
            | isNothing formatPrecision = x {formatPrecision = join $ digits <$> sizeOf a}
            | otherwise = x
          where
            digits n = case formatSpecifier of
                Bin      -> Just n
                Oct      -> Just $ (n + 2) `div` 3
                Dec      -> Nothing
                Hex      -> Just $ (n + 3) `div` 4
                HexUpper -> Just $ (n + 3) `div` 4
        updatePrecision _ = undefined
    update x = x

    validateAlignment x
        | isIntLiteral x = x
        | otherwise = errorMsg "Interpolated string alignment did not evaluate to an integer constant" x

    validateFormat x Nothing
        | tIsValueType (typeOf x) = x
        | otherwise = errorMsg ("The type of interpolation expression `" ++ showPretty (typeOf x) ++ "` is not a value type") x
    validateFormat x (Just Format{..})
        | tIsInt (typeOf x) || tIsEnum (typeOf x) || tIsFloat (typeOf x) = x
        | tIsBool (typeOf x) && formatSpecifier == Bin = x
        | otherwise = errorMsg ("Invalid format specifier `" ++ show formatSpecifier ++ "` for the value of type `" ++ showPretty (typeOf x) ++ "`") x

interpolatedStringPost e = e

resolveNames :: SymbolMap n (ParseError s e) -> DesugarAlgebra n (ParseError s e)
resolveNames symbols e@(QualifiedIdentifierF a) = case lookupSymbol a symbols of
    Just x
        | isFunction x || isEnumConstant x || isGlobalVar x -> QualifiedIdentifierF $ makeQualified x a
        | getQualifier a `isPrefixOf` getScope a -> makeUnqualified e
    _ -> e
  where
    isEnumConstant EnumConstant{} = True
    isEnumConstant _ = False
    isGlobalVar (Variable _ _ _ _ _ DeclareGlobal) = True
    isGlobalVar _ = False
resolveNames _ (FunctionSpecifierF (Just a) b) = FunctionSpecifierF (Just a) (renote makeUnqualified b)
resolveNames _ (FunctionSpecifierF Nothing (NamedValue b)) = FunctionSpecifierF Nothing b
resolveNames _ e = e

makeUnqualified :: Exp n e -> Exp n e
makeUnqualified (QualifiedIdentifierF x) = QualifiedIdentifierF $ renote makeUnqualified x
makeUnqualified (ScopedNameF x _ y) = ScopedNameF x Nothing y
makeUnqualified x = x

returnVoid :: DesugarAlgebra (Typed n) e
returnVoid (ReturnF (Just e))
    | tIsVoid $ typeOf e = SeqF [e, copyNote (ReturnF Nothing) e]
returnVoid e = e

removeTemplate :: DesugarAlgebra (Typed n) (ParseError s e)
removeTemplate TemplateF{..} = SeqF $ diagnostics templateDecl ++ concatMap diagnostics templateParams
removeTemplate x
    | isAbbreviatedTemplate x = SeqF []
    | otherwise = removeNestedSeq x

validateExtern :: SymbolMap n (ParseError s e) -> NotedExp n (ParseError s e) -> NotedExp n (ParseError s e)
validateExtern symbols = desugar validate
  where
    validate (ExternF a b) = ExternF a $ checkExtern b
    validate e = e

    checkExtern x = case lookupSymbol x symbols of
        Just Class{} -> x
        Just (Template _ Class{}) -> x
        _ -> errorMsg "Invalid extern declaration. Only classes and class templates can be declared as external." x

trimExternClasses :: (Eq (Token s), Eq e, Semigroup n) => [NotedExp (Typed n) (ParseError s e)] -> [NotedExp (Typed n) (ParseError s e)]
trimExternClasses program = parallel (desugar externClass) program
  where
    externClass e@ClassF{}
        | Set.member (getQualifiedName e) externClasses = e { body = desugarShallow trim $ body e }
      where
        trim x@FunctionF{..} = Just $ x { body = new $ SeqF $ pure $ new $ ReturnF value }
          where
            new = flip newNote body
            value
              | Void <- funcReturnType = Nothing
              | otherwise              = Just $ new $ InitializerListF $ new $ SeqF []
        trim ClassF{} = Nothing
        trim x = Just x
    externClass e = e

    externClasses = Set.unions $ parallel (setWith extern) program
      where
        extern (Extern _ a)
          | tIsClass $ theType a = [tQualifiedName $ theType a]
        extern _ = mempty

validateExportableClasses :: (Eq (Token s), Eq e, Semigroup n) => SymbolMap (Typed n) (ParseError s e) -> [NotedExp (Typed n) (ParseError s e)] -> [NotedExp (Typed n) (ParseError s e)]
validateExportableClasses symbols program = parallel (desugar validate) program
  where
    validate (ExportTypeF _ a)
        | tIsTemplate $ typeOf a = desugarError "Type template cannot be exported. Perhaps you meant to export an instance of the template?"
    validate x@(ClassF a b c) = ClassF a b $ checkExportableClass (Map.lookup (getQualifiedName x) exportableClasses) c
    validate x = x

    -- Map of classes that must meet exportable restrictions. This includes
    -- exported/external classes and classes of public objects of exportable
    -- classes.
    -- Map keys are qualified class names and values are notes of the AST
    -- node(s) that make the class exportable (either export/extern declaration
    -- or public object member declaration).
    exportableClasses = Map.unions $ parallel (mapWith exportable) program
      where
        exportable x@(ExportType _ a) = classAndPublicObjects (note $ unfix x) $ theType a
        exportable x@(Extern _ a)     = classAndPublicObjects (note $ unfix x) $ theType a
        exportable _                  = []

        classAndPublicObjects n x | tIsClass x = (tQualifiedName x, n) : publicObjects n (tQualifiedName x)
        classAndPublicObjects n x | tIsArray x = classAndPublicObjects n $ tElement x
        classAndPublicObjects _ _              = []

        publicObjects n x = case lookupSymbolByName x symbols of
            Just (Class _ _ c) -> concat $ mapClassMembers member $ unfix c
            _ -> []
          where
            member PublicMember a = classAndPublicObjects (note (unfix a) <> n) $ typeOf a
            member _ _ = []

    checkExportableClass (Just n) = renote (mapClassMembers check)
      where
        check PublicMember x@Variable{}
            | tIsFunction $ typeOf x                    = err x "Public callback"
            | otherwise                                 = err x "Public field"
        check PublicMember x@(Function _ _ _ m _ _ _ _)
            | (Just (FunctionModifier Inline)) <- m     = err x "Public inline method"
            | tIsFunction $ typeOf x                    = checkAttr x
        check _ x = x
        checkAttr x = invalidAttrErr $ Set.filter (not . valid) $ tFunctionAttr $ typeOf x
          where
            valid (TFlagAttr Async)          = True
            valid (TFlagAttr NoBackPressure) = True
            valid (TFlagAttr Pure)           = True
            valid (TFlagAttr Reset)          = True
            valid (TIntAttr Latency _)       = True
            valid (TIntAttr MaxThreads _)    = True
            valid (TIntAttr ThreadRate _)    = True
            valid _                          = False
            invalidAttrErr a
                | Set.null a = x
                | otherwise = err x ("Public method with [[" ++ showPretty (Set.elemAt 0 a) ++ "]] attribute")
        err x e = NotedExp (note (unfix x) <> n) $ desugarError (e ++ " is not allowed in an exportable class")
    checkExportableClass _ = id

staticAssert :: DesugarAlgebra (Typed n) (ParseError s e)
staticAssert (StaticAssertF a) = case a of
    BoolLiteral True  -> SeqF []
    BoolLiteral False -> desugarError "static assertion failed"
    _                 -> StaticAssertF $ errorMsg "static assertion expression did not evaluate to a boolean constant" a
staticAssert e = e

unresolved :: DesugarAlgebra (Typed n) (ParseError s e)
unresolved (ByteOffsetF t f) = offsetErr t f
unresolved (BitOffsetF t f)  = offsetErr t f
unresolved (UnaryF Clog2 _)  = desugarError "clog2 argument did not evaluate to a positive integer constant"
unresolved ParamIntF{}       = desugarError "int width did not evaluate to a positive integer constant"
unresolved ParamUintF{}      = desugarError "uint width did not evaluate to a positive integer constant"
unresolved StaticIfF{}       = desugarError "static if predicate did not evaluate to a boolean constant"
unresolved e = e

unresolvedTemplates :: Semigroup n => SymbolMap (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
unresolvedTemplates symbols = para unresolvedTemplate
  where
    unresolvedTemplate _ (Note _ (QualifiedIdentifierF e@TemplateInstance{})) = unresolvedTemplateErrors e $ lookupSymbol e symbols
    unresolvedTemplate _ (Note _ (TypeIdentifierF _ e@TemplateInstance{})) = unresolvedTemplateErrors e $ lookupSymbol e symbols
    unresolvedTemplate t@Template{} _ = t
    unresolvedTemplate _ e = Fix e

    unresolvedTemplateErrors e@(TemplateInstance _ args@(Seq args')) (Just (Template params t)) = instanceError (diagnostics args ++ paramErrors)
      where
        err msg = errorMsgWithContext (note $ unfix t) msg e
        paramErrors = case comparing length args' params of
            LT -> pure $ err $ "Could not deduce all arguments for template `" ++ getName e ++ "`"
            GT -> pure $ err $ "Too many arguments specified for template `" ++ getName e ++ "`"
            EQ -> mapMaybe unresolveArgError $ zip params args'

        unresolveArgError (p, a)
            | Error{} <- a = Nothing
            | templateArgResolved a = Nothing
            | otherwise = Just $ errorMsg ("Unresolved argument for the parameter " ++ getName p ++ " of template " ++ getName e) a

        instanceError [] = err "Failed to instantiate template"
        instanceError xs = NotedExp (note $ unfix e) $ SeqF xs
    unresolvedTemplateErrors e _ = errorMsg ("Undefined template `" ++ getName e ++ "`") e

unresolvedFunctionTemplateArgs :: NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
unresolvedFunctionTemplateArgs = para unresolvedTemplateCall
  where
    unresolvedTemplateCall _ (Note n (FunctionCallF a b@(FunctionSpecifier _ (QualifiedIdentifier TemplateInstance{})) c))
        = NotedExp n $ FunctionCallF a b $ renote (fmap unresolvedTypeError) c
    unresolvedTemplateCall t@Template{} _ = t
    unresolvedTemplateCall _ e = Fix e

    unresolvedTypeError e
        | typeResolved e = e
        | otherwise = copyNote (desugarError "Unresolved argument") e

offsetErr :: NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> Exp (Typed n) (ParseError s e)
offsetErr t f
  | tIsStruct $ theType t = BitOffsetF t (errorMsg "Undefined field" f)
  | otherwise             = BitOffsetF (errorMsg "Field offset is only defined for structs" t) f

-- Replace `ThisF` with reference to a variable representing captured `this`
-- if one is defined in current context.
thisReference :: SymbolMap (Typed n) (ParseError s e) -> DesugarAlgebra (Typed n) (ParseError s e)
thisReference symbols e@(ThisF n) = case lookupSymbol e symbols of
    Just (Variable _ _ _ a _ _) -> NamedValueF $ localIdentifier n a
    _                           -> e
thisReference _ e = e
