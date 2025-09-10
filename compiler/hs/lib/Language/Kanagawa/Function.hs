{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Function
    ( moveFunctions
    , desugarLambdas
    , abbreviatedFunctionTemplate
    , higherOrderFunctions
    , addLambdaWrappers
    ) where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.String
import Language.Kanagawa.Type
import Language.Kanagawa.Symbols
import Language.Kanagawa.Recursion
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Error
import Language.Kanagawa.Desugar

moveToParentScope ::  NotedExpF (Typed n) e (NotedExp (Typed n) e) -> (NotedExp (Typed n) e -> Bool) -> NotedExp (Typed n) e
moveToParentScope (Note n parent) cond
    | null selected = NotedExp n parent
    | otherwise = NotedExp n $ SeqF $ map move selected ++ [NotedExp n remaining]
  where
    symbols = getSymbols'' AllSymbols $ body parent

    (selected, rest) = partition cond $ toList $ unfix $ desugar fixScopedName $ desugar qualifyName $ body parent

    remaining = parent { body = copyNote (SeqF rest) $ body parent }

    mangleName e = fromString $ getName parent ++ "@" ++ getName e
    mangleIdentifier e = copyNote (IdentifierF $ mangleName e) e

    move e = renote mangleDeclName $ desugar (memberAccess . modifyDeclScopeName newScope) e
      where
        mangleDeclName (TemplateF a b) = TemplateF a $ renote mangleDeclName b
        mangleDeclName x = x { declName = mangleIdentifier $ declName x }

        newQualifiedName = getQualifier parent ++ [mangleName e]
        newScope s
            | s == getQualifier e = getQualifier parent
            | otherwise = maybe s (newQualifiedName ++) $ stripPrefix (getQualifiedName e) s

        memberAccess x@(NamedValueF a) = case lookupSymbol a symbols of
            Just (Variable _ _ (Const FunctionType{}) name _ DeclareClass) -> MemberAccessF (thisValue a) name
            Just (Variable _ _ Const{} name (Just InitializerList{}) DeclareClass) -> MemberAccessF (thisValue a) name
            Just (Variable _ _ Const{} _ (Just v) DeclareClass) -> unNote $ unfix v
            Just (Variable _ _ _ name _ DeclareClass) -> MemberAccessF (thisValue a) name
            _ -> x
        memberAccess x@(FunctionCallF a (FunctionSpecifier Nothing b) c) = case lookupSymbol b symbols of
            Just (Function _ MemberFunction _ _ _ _ _ _) -> FunctionCallF a (thisMethod b) c
            Just (Template _ (Function _ MemberFunction _ _ _ _ _ _)) -> FunctionCallF a (thisMethod b) c
            Just (Variable _ _ _ _ _ DeclareClass) -> FunctionCallF a (thisMethod b) c
            _ -> x

        memberAccess x = x

    thisValue x = thisNamedValue (getScope x) x
    thisMethod x = functionSpecifier (Just $ thisValue x) x

    selectedQualifiedNames :: [QualifiedName]
    selectedQualifiedNames = map getQualifiedName selected

    qualifyName (QualifiedIdentifierF x) = QualifiedIdentifierF $ renote qualify x
      where
        qualify (TemplateInstanceF a b) = TemplateInstanceF (renote qualify a) b
        qualify (ScopedNameF a Nothing b) = ScopedNameF a (scopedQualifier a x $ concatMap getQualifier (lookupSymbol x symbols)) b
        qualify e = e
    qualifyName e = e

    -- fix scoped names to point to moved items
    fixScopedName e@(ScopedNameF a _ b) | getQualifiedName e `elem` selectedQualifiedNames =
        ScopedNameF a (scopedQualifier a b $ getQualifier parent) $ mangleIdentifier b
    fixScopedName e = e

moveFunctions :: (Eq (Token s), Eq e) => NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
moveFunctions = para move
  where
    -- defer moves until template is instantiated
    move t@Template{} _ = t
    move t@Function{} _
        | isAbbreviatedTemplate t = t
    move t@StaticIf{} _ = t
    move t@StaticFor{} _ = t
    move t@Class{} _
        | isLocalScope t = t

    -- move functions and classes declared within a function to parent scope
    move _ e@(Note _ FunctionF{}) = moveToParentScope e functionsAndClasses

    move _ e@(Note _ NestedScopeF{}) = moveToParentScope e functionsAndClasses

    -- move lambda "methods" to parent scope
    move _ e@(Note _ ClassF{}) = moveToParentScope e lambdaFunctions

    move _ (Note n (DoWhileF a b (Seq c)))
        | not $ null c = NotedExp n $ SeqF (init c ++ [NotedExp n $ DoWhileF a b $ last c])

    move _ (Note n (RangeForF a b c (Seq d)))
        | not $ null d = NotedExp n $ SeqF (init d ++ [NotedExp n $ RangeForF a b c $ last d])

    move _ (Note n (IfF a (Seq b) (Just (Seq c))))
        | not (null b || null c) = NotedExp n $ SeqF (init b ++ init c ++ [NotedExp n $ IfF a (last b) (Just $ last c)])

    move _ (Note n (IfF a (Seq b) c))
        | not $ null b = NotedExp n $ SeqF (init b ++ [NotedExp n $ IfF a (last b) c])

    move _ (Note n (IfF a b (Just (Seq c))))
        | not $ null c = NotedExp n $ SeqF (init c ++ [NotedExp n $ IfF a b (Just $ last c)])

    move _ (Note n e) = NotedExp n (removeNestedSeq e)

    functionsAndClasses Function{} = True
    functionsAndClasses (Template _ Function{}) = True
    functionsAndClasses Class{} = True
    functionsAndClasses _ = False

    lambdaFunctions (Function _ LambdaFunction _ _ _ _ _ _) = True
    lambdaFunctions (Template _ (Function _ LambdaFunction _ _ _ _ _ _)) = True
    lambdaFunctions _ = False

typeParam :: Exp n e -> Exp n e
typeParam (TemplateParamF a b c d _) = TypeParamF a b c d
typeParam _ = undefined

resolvedFuncParams :: SymbolMap n (ParseError s e) -> NotedExp n (ParseError s e) -> [NotedExp n (ParseError s e)]
resolvedFuncParams symbols = map (renote resolveFuncParam) . toList . unfix
  where
    resolveFuncParam (FuncParamF a b c d) = FuncParamF a b (resolveAlias c) d
    resolveFuncParam e = e

    resolveAlias e@TypeIdentifier{} = case lookupSymbol e symbols of
        (Just (Alias _ t _)) -> resolveAlias t
        _ -> e
    resolveAlias e = e

makeCapture :: Exp (Typed n) e -> Exp (Typed n) e
makeCapture LambdaF{..} = FunctionCallF (newSeq []) specifier $ renote (fmap arg) lambdaCaptures
  where
    e            = lambdaCaptures
    new          = flip newNote e
    newSeq       = new . SeqF . map new
    specifier    = new $ FunctionSpecifierF Nothing ident
    ident        = qualifiedIdentifier declScopeName make_capture e
    make_capture = [ if null (unfix lambdaCaptures) then data'closure'core else data'closure
                   , indexedName (length $ unfix lambdaCaptures) "make_capture"
                   ]
    arg x@This{}        = x
    arg x@(Capture _ a) = newNote (NamedValueF (localIdentifier declScopeName a)) x
    arg _               = undefined
makeCapture _ = undefined

makeClosure :: Exp (Typed n) e -> Maybe (Exp (Typed n) e) -> Exp (Typed n) e -> Exp (Typed n) e -> Exp (Typed n) e
makeClosure LambdaF{..} lambdaType captureType capture = CastF closure $ new capture
  where
    e            = lambdaCaptures
    new          = flip newNote e
    closure      = closureInstance declScopeName args e
    args         = [fromMaybe (decltype lambda) lambdaType, lambda, captureType]
    decltype     = DecltypeF declScopeName . new
    lambda       = QualifiedIdentifierF $ new $ ScopedNameF declScopeName (scopedQualifier declScopeName e declScopeName) declName
makeClosure _ _ _ _ = undefined


templateInstance :: QualifiedName -> QualifiedName -> [Exp (Typed n) e] -> NotedExp (Typed n) e -> NotedExp (Typed n) e
templateInstance declScopeName templateName args e = new $ TemplateInstanceF template $ newSeq args
  where
    new       = flip newNote e
    newSeq    = new . SeqF . map new
    template  = fromJust $ scopedQualifier declScopeName e templateName

closureInstance :: QualifiedName -> [Exp (Typed n) e] -> NotedExp (Typed n) e -> NotedExp (Typed n) e
closureInstance declScopeName args e = new $ TypeIdentifierF declScopeName instance_
  where
    new       = flip newNote e
    instance_ = templateInstance declScopeName data'closure'core'Closure args e

isGenericFunction :: Exp n e -> Bool
isGenericFunction = any (isAuto . paramType . unNote . unfix) . unfix . funcParams

desugarLambdas :: NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
desugarLambdas = desugar' expand
  where
    expand _ e@FunctionF{..}          = e { body = lambdaDefsAndRefs body }
    expand _ e@LambdaF{..}            = e { body = lambdaDefsAndRefs body }
    expand n e@VariableF{..}
      | declFlags == DeclareGlobal    = unNote $ unfix $ lambdaDefsAndRefs $ NotedExp n $ SeqF [NotedExp n e]
    expand _ e@StaticIfF{..}          = e { staticIfThen = renote go staticIfThen
                                        , staticIfElse = renote go <$> staticIfElse
                                        }
      where
        go e'@NestedScopeF{..} = e' { body = lambdaDefsAndRefs body }
        go e' = e'
    expand _ e = removeNestedSeq e

    lambdaDefsAndRefs e = desugarShallow lambdaRef $ foldr cons e $ lambdaDefs e

    lambdaRef l@LambdaF{..}
        | isGenericFunction l = Just $ makeClosure l (Just genericFunctionType) captureType $ makeCapture l
        | otherwise           = Just $ makeClosure l Nothing captureType $ makeCapture l
      where
        new = flip newNote lambdaCaptures
        newSeq = new . SeqF . map new
        genericFunctionType = FunctionTypeF (newSeq []) (newSeq params) funcReturnType
        params = captureParam : map functionTypeParam (toList $ unfix funcParams)
        captureType = captureTypeIdent declScopeName $ new l
        captureParam = FunctionTypeParamF (newSeq []) (new captureType) Nothing
        functionTypeParam p = FunctionTypeParamF (paramAttr $ unNote $ unfix p) (paramType $ unNote $ unfix p) Nothing

    lambdaRef FunctionF{}   = Nothing
    lambdaRef StaticForF{}  = Nothing
    lambdaRef e             = Just e

    captureTypeName l = declName (unNote $ unfix l) `indetifierAppend` "$capture"

    captureTypeIdent scope l = TypeIdentifierF scope (newNote scopedName l)
      where
        scopedName = addQualifierFrom l (ScopedNameF scope Nothing (captureTypeName l))

    lambdaDefs = concatMap lambdaDef . para lambda
      where
        lambda l@Lambda{}  = const [l]
        lambda Function{}  = const []
        lambda StaticIf{}  = const []
        lambda StaticFor{} = const []
        lambda _           = fold

        lambdaDef l@(NotedExp _ LambdaF{..}) = [captureTypeAlias, lambdaFunction]
          where
            emptySeq = newNote (SeqF []) l
            attr = emptySeq

            captureTypeAlias = newNote (AliasF declScopeName typ $ captureTypeName l) l
              where
                typ = newNote (DecltypeF declScopeName $ renote makeCapture l) l

            -- declaration of local function corresponding to a lambda
            lambdaFunction =
                newNote (FunctionF declScopeName LambdaFunction attr inline funcReturnType declName (captureParam `cons` funcParams) lambdaBody) l
              where
                inline = Just $ newNote (FunctionModifierF Inline) l
                signatureDeclScopeName = declScopeName ++ [getName l]
                bodyDeclScopeName = functionScope signatureDeclScopeName
                captureParamName = declName `indetifierAppend` "$capture_param"
                captureParam = newNote (FuncParamF signatureDeclScopeName attr captureType captureParamName) l
                captureType = newNote (captureTypeIdent signatureDeclScopeName l) l
                lambdaBody = foldr (cons . flip newNote l . uncaptureVar) body $ zip [0..] $ toList $ unfix lambdaCaptures
                uncaptureVar (i, x) = VariableF bodyDeclScopeName attr (new AutoF) varName (Just captureAccess) DeclareLocal
                  where
                    new = flip newNote x
                    varName       = new $ IdentifierF $ getName x
                    captureAccess = new $ MemberAccessF capturesValue $ new $ indexedIdentifier i "x"
                    capturesValue = new $ NamedValueF $ localIdentifier bodyDeclScopeName captureParamName
        lambdaDef _ = []

mergeTemplateParams :: [NotedExp n e] -> [NotedExp n e] -> NotedExp n e -> Exp n e
mergeTemplateParams p1 p2 e = TemplateF (map reindexTypeParams p1 ++ map reindexTemplateParam p2) (reindexTypeParams e)
  where
    offset = length p1

    reindexTemplateParam = desugar reindex
      where
        reindex (TemplateParamF a b c d e') = TemplateParamF a b c (d + offset) e'
        reindex x = reindexTypeParam x

    reindexTypeParams = desugar reindexTypeParam

    reindexTypeParam (TypeParamF a b c d)
        | any paramMatches p2 = TypeParamF a b c (d + offset)
      where
        paramMatches (TemplateParam _ _ c' d' _) = c == c' && d == d'
        paramMatches _ = False
    reindexTypeParam x = x

findByName :: (Foldable t, ExpLike e1, ExpLike e2) => e1 -> t (a, e2) -> Maybe (a, e2)
findByName e = find byName
  where
    byName (_, x) = getName e == (getName x :: String)

abbreviatedFunctionTemplate :: Typed n -> Exp (Typed n) (ParseError s e) -> Maybe (Exp (Typed n) (ParseError s e))
abbreviatedFunctionTemplate n f
    | isAbbreviatedTemplate f = Just $ SeqF [ NotedExp n $ TemplateF templateParams $ NotedExp n f'
                                            , NotedExp n f
                                            ]
  where
    autoParams = zip [0..] $ filter (isAuto . paramType . unNote . unfix) $ toList $ unfix $ funcParams f

    templateParams = map (\x -> newNote (templateParam x) $ snd x) autoParams

    templateParam (i, e@(FuncParam declScopeName _ _ (Identifier name))) =
        TemplateParamF (init declScopeName) (newNote TypenameF e) (suffixName "$T" name) i Nothing
    templateParam _ = undefined

    findParam e = fromJust $ findByName e autoParams

    f' = f {funcParams = desugar autoParamType $ funcParams f}
      where
        autoParamType p@FuncParamF{..}
            | isAuto paramType = p {paramType = newNote (typeParam $ templateParam $ findParam p) paramType}
        autoParamType p = p

abbreviatedFunctionTemplate _ TemplateF{} = Nothing
abbreviatedFunctionTemplate _ e = Just $ removeNestedSeq e

higherOrderFunctions :: SymbolMap (Typed n) (ParseError s e) -> Typed n -> DesugarAlgebra (Typed n) (ParseError s e)
higherOrderFunctions symbols n f@FunctionF{..}
    | not $ null functionTypeParams = functionTemplate funcModifier
    | otherwise = f
  where
    isFunctionType FunctionType{} = True
    isFunctionType _ = False

    functionTypeParams = zip [0..] $ filter (isFunctionType . paramType . unNote . unfix) $ resolvedFuncParams symbols funcParams

    -- desugar higher order inline function into a template
    functionTemplate (Just (FunctionModifier Inline)) = TemplateF (concatMap templateParams functionTypeParams) (NotedExp n function)
    functionTemplate _ = desugarError "Non-inline higher order functions are not supported"

    templateParams x = map new
        [ captureTemplateParam
        , closureFunctionTemplateParam
        ]
      where
        new fn = newNote (fn x) (snd x)

    closureFunctionType x@(_, FuncParam _ _ (FunctionType a b c) _) =
        FunctionTypeF a (cons (captureParam b) b) c
      where
        captureParam e = new $ FunctionTypeParamF (new $ SeqF []) (new $ captureTypeParam x) Nothing
          where
            new = flip newNote e
    closureFunctionType _ = undefined

    captureTemplateParam (i, e@(FuncParam _ _ _ (Identifier name))) =
        TemplateParamF declScopeName (newNote TypenameF e) (suffixName "$capture" name) (i * 2) Nothing
    captureTemplateParam _ = undefined

    closureFunctionTemplateParam x@(i, e@(FuncParam _ _ _ (Identifier name))) =
        TemplateParamF declScopeName (newNote (closureFunctionType x) e) name (i * 2 + 1) Nothing
    closureFunctionTemplateParam _ = undefined

    captureTypeParam = typeParam . captureTemplateParam
    closureFunctionTypeParam = typeParam . closureFunctionTemplateParam

    function = f { funcParams = renote (fmap $ renote rewrite) funcParams }
      where
        rewrite x = maybe x setClosureParamType $ findByName x functionTypeParams
          where
            setClosureParamType p@(_, FuncParam a b c d) =
                FuncParamF a b (closureInstance a [closureFunctionType p, closureFunctionTypeParam p, captureTypeParam p] c) d
            setClosureParamType _ = undefined

higherOrderFunctions _ _ (TemplateF p1 (Template p2 e)) = mergeTemplateParams p1 p2 e

higherOrderFunctions _ _ e = e

-- wrap functions passed as arguments to higher order functions into lambdas
addLambdaWrappers :: SymbolMap (Typed n) (ParseError s e) -> DesugarAlgebra (Typed n) (ParseError s e)
addLambdaWrappers symbols (FunctionCallF a b c) = FunctionCallF a b (renote (fmap $ wrapFunctionArg (getScope b)) c)
  where
    wrapFunctionArg declScopeName e = case e of
        NamedValue q                  -> wrap q
        MemberAccess o f
            | tIsReference $ typeOf o -> wrap $ qualifiedIdentifier [] (tQualifiedName (typeOf o) ++ [getName f]) o
        _                             -> e
      where
        new = flip newNote e
        newSeq = new . SeqF . map new
        emptySeq = newSeq []

        paramsFromFunctionType t = new $ SeqF paramList
          where
            paramList     = zipWith funcParam [0..] $ tFunctionParams t
            funcParam i p = new $ FuncParamF declScopeName (paramAttr p) (paramType p) (paramName p)
              where
                paramAttr = newSeq . expFromTAttr e . attrOfParam
                paramType = new . fromJust . expFromType e . typeOfParam
                paramName = new . maybe (indexedIdentifier i "$wrapper_param") IdentifierF . nameOfParam

        wrap q = case lookupSymbol q symbols of
            Just (Function _ kind _ _ ret _ params _)                 -> wrapper kind params ret
            Just (FunctionDecl _ _ ret _ params)                      -> wrapper FreeFunction params ret
            Just (Variable _ _ t _ _ flags) | tIsFunction (theType t) -> wrapper (kindFromFlag flags)
                                                                            (paramsFromFunctionType $ theType t)
                                                                            (new $ fromJust $ expFromType e $ tReturn $ theType t)
            _ -> e
          where
            kindFromFlag x = if x == DeclareClass then MemberFunction else FreeFunction

            wrapper kind params ret = new $ LambdaF declScopeName lambdaIdent lambdaCaptures lambdaParams ret lambdaBody
              where
                lambdaName = fromString $ concatMap ('$':) ("wrapper" : getQualifiedName q)
                lambdaIdent = new $ IdentifierF lambdaName

                lambdaSignatureDeclScopeName = declScopeName ++ [lambdaName]
                lambdaBodyDeclScopeName = functionScope lambdaSignatureDeclScopeName
                lambdaParams = renote (fmap $ renote $ modifyDeclScopeName (const lambdaSignatureDeclScopeName)) params

                lambdaCaptures
                    | kind == MemberFunction = newSeq [ThisF declScopeName]
                    | otherwise              = emptySeq

                lambdaBody = newSeq [ReturnF $ Just functionCall]
                functionCall = new $ FunctionCallF emptySeq (functionSpecifier object $ desugar updateScope q) (renote (fmap $ renote functionArg) lambdaParams)
                  where
                    updateScope x@TypeParamF{} = x
                    updateScope x = modifyDeclScopeName (const lambdaBodyDeclScopeName) x

                    object
                        | kind == MemberFunction = Just $ thisNamedValue lambdaBodyDeclScopeName e
                        | otherwise              = Nothing

                    functionArg x = NamedValueF (localIdentifier lambdaBodyDeclScopeName (declName x))

addLambdaWrappers _ e = e
