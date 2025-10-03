{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.TypeCheck
    ( validateProgram
    , templateTypeCheck
    , templateResolved
    , templateArgResolved
    , templateArgsResolved
    , resolveTemplateParam
    , resolveTypeParam
    ) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Language.Kanagawa.Constexpr
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Internal
import Language.Kanagawa.Mangle
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Language.Kanagawa.Type
import Language.Kanagawa.Type.Inference

validateProgram :: (Eq (Token s), Eq e, Semigroup n) => SymbolMap (Typed n) (ParseError s e) -> Typed n -> DesugarAlgebra (Typed n) (ParseError s e)
validateProgram symbols note = check
  where
    showUnmangled = show . unmangle symbols

    check x@(UnaryF BitSizeof _)     = x
    check x@(UnaryF ByteSizeof _)    = x
    check x@UnaryF{}                 = checkInvalidType x
    check x@BinaryF{}                = checkInvalidType x
    check x@FanOutF{}                = checkInvalidType x
    check x@MuxF{}                   = checkInvalidType x
    check x@ParamIntF{}              = checkInvalidType x
    check x@ParamUintF{}             = checkInvalidType x
    check (FunctionCallF a b c)      = FunctionCallF a b $ renote (checkFunctionArgs $ typeOf b) c
    check (ArrayF a b c)             = ArrayF a b $ checkArraySize c
    check x@IntAttrF{}               = fmap errorIfNotInteger x
    check (EnumF a b c d)            = EnumF a b (checkEnumBaseType c) (checkEnumConsts c d)
    check x@VariableF{}              = checkShadowDecl $ checkVarDecl x
    check AutoF{}                    = desugarError "Could not deduce type"
    check LambdaF{}                  = desugarError "Invalid use of lambda expression in this context"
    check x@DecltypeF{}              = fmap (errorMsg "Couldn't deduce type of the expression") x
    check x@(AssignF a _)            = checkConstLValue x a
    check x@FunctionF{}              = checkFunctionType x $ typeOf note
    check (DefaultInitializationF a) = DefaultInitializationF $ checkDesignatedInitializer a
    check (InitializerListF a)       = InitializerListF $ renote overridingInitializerWarning a
    check x                          = x

    overridingInitializerWarning (SeqF xs) = SeqF $ zipWith dupWarning xs $ inits xs
      where
        dupWarning x ys = case find (dupDesignator x) $ reverse ys of
            Just y  -> warningMsg InitializerOverrides ("Initializer overrides prior initialization of `" ++ getName y ++ "`") $ noteContext y x
            Nothing -> x
        dupDesignator (Designator a _) (Designator b _) = a == b
        dupDesignator _ _ = False
    overridingInitializerWarning _ = undefined

    checkDesignatedInitializer a
        | not $ tIsDesignatedInitializer $ typeOf a = errorMsg "Expected designated initializer" a
        | otherwise = a

    checkFunctionType e t
        | not $ tIsFunction t         = e
        | any reset $ tFunctionAttr t = checkReset
        | otherwise                   = e {body = renote (fmap $ renote checkReturn) $ body e}
      where
        reset (TFlagAttr Reset) = True
        reset _ = False
        checkReset
            | not $ null $ tFunctionParams t = desugarError "Function with [[reset]] attribute can't have parameters"
            | not $ tIsVoid $ tReturn t      = desugarError "Function with [[reset]] must have void return type"
            | otherwise                      = e
        checkReturn (ReturnF (Just x)) = ReturnF $ Just $ checkConversion allowNarrowingConversions (tReturn t) x
        checkReturn x = x

    checkFunctionArgs t (SeqF args) | tIsFunction t = checkArguments (tFunctionAttr t) (tFunctionParams t) args
    checkFunctionArgs _ e = e

    checkArguments attr tParams args
        | length tParams /= length args = desugarError ("Invalid number of arguments, expected " ++ show (length tParams))
        | Set.member pipelined attr       -- don't type-check the first argument of [[pipelined]] functions
          && not (null args)            = SeqF $ head args : zipWith checkArg (tail tParams) (tail args)
        | otherwise                     = SeqF $ zipWith checkArg tParams args
      where
        pipelined = TFlagAttr Pipelined
        checkArg tParam arg = checkConversion warningNarrowingConversions (typeOfParam tParam) arg

    conversionError s t hint = errorMsg (("Invalid conversion from `" ++ showUnmangled s ++ "` to `" ++ showUnmangled t ++ "`") ++ hint)

    warningNarrowingConversions s t = warningMsg Conversion ("Narrowing conversion from `" ++ showUnmangled s ++ "` to `" ++ showUnmangled t ++ "`")
    allowNarrowingConversions _ _ = id
    errorNarrowingConversions s t = conversionError s t ("\nInsert an explicit cast to silence this issue `cast<" ++ showUnmangled t ++ ">()`")

    checkConstLValue x a = checkType $ typeOf a
      where
        checkType t | tIsConst t = desugarError $ "`" ++ showPretty a ++ "`: illegal use of this constant as an lvalue"
                    | otherwise = x

    checkShadowDecl a@VariableF{..}
        | null declScopeName = a
        | declFlags == DeclareMember || declFlags == DeclareClass = a
        | otherwise = case lookupInScope (init declScopeName) declName symbols of
              Just x | shadowable x
                -> a { declName = warningMsg Shadow ("This declaration of `" ++ getName a ++ "` shadows the existing declaration") $ noteContext x declName }
              _ -> a
      where
        shadowable x@Variable{} = not $ null $ getScope x
        shadowable FuncParam{} = True
        shadowable _ = False
    checkShadowDecl a = a

    checkVarDecl a@VariableF{..} = checkVar (theType varType) (tUnConst . typeOf <$> varInit)
      where
        checkVar t _
            | tResolved t && not (tIsConst t) && declFlags == DeclareGlobal = desugarError "Global state is not allowed"
        checkVar t (Just s)
            | tIsEccMem t = desugarError "ECC memories with initial values are not supported"
            | tIsClosure s && declFlags == DeclareGlobal = a { varInit = reduceClosure s <$> varInit }
            | (Just (InitializerList (Seq (Designator{}:_)))) <- varInit = initValueCheck errorNarrowingConversions t a
            | tResolved s && tResolved t && not (tIsError s) && not (tIsFunction t) = initValueCheck warningNarrowingConversions t a
        checkVar t _
            | not $ tIsFunction t = a
        checkVar t Nothing
            | tIsConst t = desugarError "A constant of a function type must be initialized"
        checkVar t (Just s)
            | not $ s <: t = initValueCheck errorNarrowingConversions t a
        checkVar t _
            | (declFlags /= DeclareClass) && tIsConst t = SeqF []
            | declFlags /= DeclareClass = desugarError "Expected 'const'. Mutable variables of a function type are not allowed"
              -- middle-end doesn't handle function type aliases, convert to explicit function type
            | TypeIdentifier{} <- varType = a { varType = maybe varType (`copyNote` varType) (expFromType varType t) }
            | otherwise = a
    checkVarDecl a = a

    reduceClosure s e
        | tWidth (tClosureCapture s) == Just 0 = copyNote (InitializerListF $ copyNote (SeqF []) e) e
        | otherwise                            = errorMsg "Closures that capture value(s) are not allowed in global scope" e

    initValueCheck n t a = a { varInit = checkConversion n t <$> varInit a }

    checkConversion _ t a
        | tIsError t                       = errorMsg (showPretty t) a
    checkConversion n t a@(IntLiteral _ x) = checkConversion' n (inferIntLiteralType Nothing x) t a
    checkConversion n t (EnumValue _ x)
        | tIsInt t                         = checkConversion n t x
    checkConversion _ t a@(Designator x y) = copyNote (DesignatorF x $ checkConversion errorNarrowingConversions t y) a
    checkConversion n t a@(Positional x y) = copyNote (PositionalF x $ checkConversion n t y) a
    checkConversion n t a                  = checkConversion' n (typeOf a) t a

    checkConversion' _ s t a
        | not $ null $ errors a = a
        | tIsError s = errorMsg (showPretty s) a
        | s <: t     = a
    checkConversion' n _ t a@InitializerList{}
        | tIsUnion t = renote (fmap $ renote checkUnionInit) a
        | otherwise  = renote (fmap $ renote $ fmap checkInitItem) a
      where
        checkUnionInit (SeqF (x:xs)) = SeqF $ checkInitItem x : excessItem xs
            where
              excessItem []       = []
              excessItem (x':xs') = errorMsg "Excess elements in union initializer" x' : xs'
        checkUnionInit e = e

        checkInitItem e = checkConversion n (tInitializedType t $ typeOf e) e
    checkConversion' narrowing s t a
        | s `tIsConvertibleTo` t = narrowing s t a
        | otherwise = conversionError s t "" a

    checkInvalidType = fmap errorIfType
      where
        errorIfType a
            | isType a = errorMsg "Invalid use of a type as an expression" a
        errorIfType a@Seq{} = renote checkInvalidType a
        errorIfType a = a

    checkArraySize = renote (fmap errorIfNotPositive)
      where
        arraySizeError = errorMsg "Array dimension must be a positive integer"
        errorIfNotPositive a@(IntLiteral _ n)
            | n > 0 = a
            | otherwise = arraySizeError a
        errorIfNotPositive a@(EnumValue _ (IntLiteral _ n))
            | n > 0 = a
            | otherwise = arraySizeError a
        errorIfNotPositive a = arraySizeError a

    errorIfNotInteger a@(IntLiteral _ x) | x >= 0 = a
    errorIfNotInteger a = errorMsg "Expected a constant expression that evaluates to non-negative integer" a

    checkEnumBaseType a
        | tIsInt $ theType a = copyNote (fromJust $ expFromType a $ theType a) a
        | otherwise          = errorMsg "Enums must be based on an integral type" a

    checkEnumConsts (Integer n) = renote (fmap $ errorIfOutOfRange (-(2 ^ (n - 1))) (2 ^ (n - 1)))
    checkEnumConsts (Unsigned n) = renote (fmap $ errorIfOutOfRange 0 (2 ^ n))
    checkEnumConsts _ = id

    errorIfOutOfRange begin end e@(NotedExp _ (EnumConstantF _ _ (Just x))) = errorIfOutOfRange' x
      where
        errorIfOutOfRange' (NotedExp _ (EnumValueF _ a)) = errorIfOutOfRange' a
        errorIfOutOfRange' a@(IntLiteral _ n)
            | begin <= n && n < end                      = e
            | otherwise                                  = errorMsg "Enum constant value is out of range for the base type" a
        errorIfOutOfRange' a                             = errorMsg "Enum constant value doesn't evaluate to an integer constant" a
    errorIfOutOfRange _ _ _ = undefined

templateTypeCheck :: (Eq (Token s), Eq e) => SymbolMap (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
templateTypeCheck symbols = desugar check
  where
    showUnmangled = show . unmangle symbols

    check (TemplateInstanceF a b) | templateResolved symbols template' b = TemplateInstanceF a (renote (validateArgs template') b)
      where
        template' = lookupSymbol a symbols
        validateArgs (Just template@(Template params _)) (SeqF args) = SeqF $ zipWith validateArg params args
          where
            validateArg (TemplateParam _ paramType paramName _ _) argExp@(NotedExp n e) = NotedExp n $
                validate (resolveTemplateParamType symbols template args paramType) e
              where
                showCommaSep = intercalate ", " . map showPretty
                argError = desugarError . (("Invalid argument for template parameter `" ++ showPretty paramName ++ "`.\n") ++)

                validate Typename NamedValueF{}
                    | tIsType $ typeOf argExp                   = argError "Expected a type; did you forget 'typename`"

                validate Typename arg
                    | tIsType $ typeOf argExp                   = arg
                    | otherwise                                 = argError "Expected a type"

                validate (Template params1 _) arg
                    | tIsTemplate $ typeOf argExp                = validateTemplate $ lookupSymbol arg symbols
                  where
                    validateTemplate (Just (Template params2 _)) = matchParams params1 params2
                      where
                        matchParams [] ys
                            | all hasDefault ys                 = arg
                        matchParams (x:xs) (y:ys)
                            | matchParamKind x y                = matchParams xs ys
                        matchParams _ _                         = argError $ "Expected a template with parameters:\n\t<" ++ showCommaSep params1 ++ ">\n" ++
                                                                             "but the argument has parameters:\n\t<" ++ showCommaSep params2 ++ ">"
                        matchParamKind t1 (TemplateParam _ t2 _ _ _)
                            | t1 == t2     = True
                            | otherwise    = theType t1 == theType t2
                        matchParamKind _ _ = False

                        hasDefault (TemplateParam _ _ _ _ Nothing)
                                           = False
                        hasDefault _       = True
                    validateTemplate _                          = argError "Expected a template"
                validate Template{} _                           = argError "Expected a template specifier"

                validate _ _
                    | tIsType $ typeOf argExp                   = argError "The type can't be used as an argument for non-type template parameter"

                validate typ@FunctionType{} arg
                    | tIsTemplate $ typeOf argExp               = arg
                    | otherwise                                 = validateFunctionType (theType typ) (tUnConst $ typeOf argExp)
                  where
                    validateFunctionType _ t
                        | tIsMethod t                           = argError "Expected a non-member function"

                    validateFunctionType t1 t2
                        | t2 <: t1                              = arg
                        | otherwise                             = argError $ "Expected a function of type:\n\t" ++ showUnmangled t1 ++ "\n" ++
                                                                             "but the argument type is:\n\t" ++ showUnmangled t2

                validate Auto InitializerListF{}                = argError "Initializer list can't be used as an argument for `auto` parameter"

                validate typ (IntLiteralF _ i)
                    | tIsSigned (theType typ) && width > 0      = IntLiteralF (Just $ LiteralType SignedLiteral width) $ truncateInt width i
                    | tIsUnsigned (theType typ) && width > 0    = IntLiteralF (Just $ LiteralType UnsignedLiteral width) $ truncateUInt width i
                    | Auto <- typ                               = IntLiteralF Nothing i
                  where
                    width = fromJust $ sizeOf typ

                validate Auto arg                               = arg

                validate typ arg                                = validateType (theType typ) (tUnConst $ typeOf argExp)
                  where
                    validateType t1 t2
                        | t2 <: t1                              = arg
                        | otherwise                             = argError $ "Expected a constant expression of type:\n\t" ++ showUnmangled t1 ++ "\n" ++
                                                                             "but the argument type is:\n\t" ++ showUnmangled t2
            validateArg _ _ = undefined
        validateArgs _ _ = desugarError $ "Undefined template " ++ getName a
    check x = x

templateResolved :: (Eq (Token s), Eq e) => SymbolMap (Typed n) (ParseError s e) -> Maybe (NotedExp (Typed n) (ParseError s e)) -> NotedExp (Typed n) (ParseError s e) -> Bool
templateResolved symbols x@(Just t@(Template params _)) (Seq args) =
    templateArgsResolved x args &&
    all paramResolved params
  where
    paramResolved (TemplateParam _ Typename _ _ _)      = True
    paramResolved (TemplateParam _ Template{} _ _ _)    = True
    paramResolved (TemplateParam _ Auto _ _ _)          = True
    paramResolved (TemplateParam _ a _ _ _)
        | typeResolved a                                = True
        | otherwise                                     = typeResolved $ resolveTemplateParamType symbols t args a
    paramResolved _                                     = False

templateResolved _ _ _ = False

templateArgResolved :: NotedExp (Typed n) e -> Bool
templateArgResolved x = or [ tIsFunction $ typeOf x
                           , tIsType     $ typeOf x
                           , tIsTemplate $ typeOf x
                           , and [ typeResolved x
                                 , constexprResolved x
                                 ]
                           ]

templateArgsResolved :: Maybe (NotedExp (Typed n) e) -> [NotedExp (Typed n) e] -> Bool
templateArgsResolved (Just (Template params _)) args
    | length params == length args = and $ zipWith resolved params args
  where
    resolved (TemplateParam _ Template{} _ _ _) = tIsTemplate . typeOf
    resolved _ = templateArgResolved
templateArgsResolved _ _ = False

resolveTypeParam :: ExpLike a => a -> [NotedExp n e] -> DesugarAlgebra n e
resolveTypeParam template args = rewrite
  where
    argAt = unNote . unfix . (args !!)
    rewrite (TypeParamF s _ _ index)
        | s == getQualifier template = argAt index
    rewrite e                        = removeNestedTypeIdentifier e

resolveTemplateInstance :: (Eq (Token s), Eq e) => SymbolMap (Typed n) (ParseError s e) -> DesugarAlgebra (Typed n) (ParseError s e)
resolveTemplateInstance symbols (TemplateInstanceF a b)
    | templateResolved symbols template b' && isJust templateInstance = instanceIdentifier
  where
    b' = inferType' symbols b
    template = lookupSymbol a symbols
    templateInstance = lookupSymbol instanceIdentifier symbols
    instanceIdentifier = mangleTemplateScopedName (unNote $ unfix $ fromJust template) b' $ unNote $ unfix a
resolveTemplateInstance _ e = e

resolveTemplateParamType
    :: ExpLike a
    => (Eq (Token s), Eq e)
    => SymbolMap (Typed n) (ParseError s e)
    -> a
    -> [NotedExp (Typed n) (ParseError s e)]
    -> NotedExp (Typed n) (ParseError s e)
    -> NotedExp (Typed n) (ParseError s e)
resolveTemplateParamType symbols template args =
    inferType' symbols
  . desugar (resolveTemplateInstance symbols)
  . templateTypeCheck symbols
  . inferType' symbols
  . desugar (resolveTypeParam template args)

resolveTemplateParam
    :: ExpLike a
    => (Eq (Token s), Eq e)
    => SymbolMap (Typed n) (ParseError s e)
    -> a
    -> [NotedExp (Typed n) (ParseError s e)]
    -> NotedExp (Typed n) (ParseError s e)
    -> NotedExp (Typed n) (ParseError s e)
resolveTemplateParam symbols template args = renote resolve
  where
    resolve (TemplateParamF a b c d e)
        | not $ typeResolved b = TemplateParamF a b' c d e
      where
        b' = resolveTemplateParamType symbols template args b
    resolve e = e
