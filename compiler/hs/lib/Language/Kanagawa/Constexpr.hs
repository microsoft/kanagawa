{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Constexpr
    ( evaluateConstexpr
    , staticControl
    , constexprResolved
    ) where

import Data.Bits (shiftL, shiftR, xor, testBit, complement, (.&.), (.|.))
import Data.Foldable
import Data.List
import Data.Maybe
import Data.String
import GHC.Float (castWord32ToFloat, castFloatToWord32)
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Internal
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Language.Kanagawa.Type

data Dependency = Independent | Dependent | DependentConstexpr
  deriving Eq

instance Semigroup Dependency where
    DependentConstexpr <> _  = DependentConstexpr
    Dependent <> Independent = Dependent
    _ <> y                   = y

instance Monoid Dependency where
    mempty = Independent

staticControl :: Algebra (NotedExpF (Typed n) (ParseError s e)) (NotedExp (Typed n) (ParseError s e))
staticControl (Note n e) = NotedExp n $ eval e
  where
    eval (StaticIfF (BoolLiteral True) x _) =
        unNote $ unfix $ removeNestedScope x

    eval (StaticIfF (BoolLiteral False) _ x) =
        maybe (SeqF []) (unNote . unfix . removeNestedScope) x

    eval (StaticForF a b c) = case inductionVarDependency of
        DependentConstexpr
            | IntLiteral _ i <- b -> NestedScopeF (init bodyDeclScopeName) (new $ IdentifierF $ last bodyDeclScopeName) $ iterations i
            | otherwise           -> e
        _                         -> UnrolledForF a b c
      where
        new = flip newNote c
        loopDeclScopeName = getQualifier a
        loopInductionName = getQualifiedName a :: [String]
        bodyDeclScopeName
            | NestedScope{} <- c = getQualifiedName c
            | otherwise          = loopDeclScopeName
        inductionVarDependency = para depends c
          where
            depends TemplateInstance{} x
                | fold x == Dependent = DependentConstexpr
            depends Array{} x
                | fold x == Dependent = DependentConstexpr
            depends ParamInt{} x
                | fold x == Dependent = DependentConstexpr
            depends ParamUint{} x
                | fold x == Dependent = DependentConstexpr
            depends StaticAssert{} x
                | fold x == Dependent = DependentConstexpr
            depends Static{} x
                | fold x == Dependent = DependentConstexpr
            depends UnrolledFor{} x
                | forLimit (unNote x) == Dependent = DependentConstexpr
            depends StaticFor{} x
                | forLimit (unNote x) == Dependent = DependentConstexpr
            depends StaticIf{} x
                | staticIfCond (unNote x) == Dependent = DependentConstexpr
            depends (Identifier y) _
                | getName a == y = Dependent
            depends _ x = fold x
        iterations limit
            | limit < 0 = copyNote (desugarError $ "Negative iteration count is invalid: " ++ show limit) b
            | otherwise = new $ SeqF $ map iteration [0..limit-1]
        iteration i = new $ NestedScopeF bodyDeclScopeName iterIdent $ new $ SeqF
            [ renote inductionVar a
            , desugar rewrite c
            ]
          where
            iter = "$" ++ getName a ++ show i
            iterName = fromString iter
            iterIdent = new $ IdentifierF iterName
            iterDeclScopeName = bodyDeclScopeName ++ [iterName]
            inductionVar v = v { declScopeName = iterDeclScopeName
                               , varInit = Just $ new $ IntLiteralF Nothing i
                               }
            rewrite = modifyDeclScopeName fixDeclScopeName . fixScopedName . replicateLambda

            fixDeclScopeName x
                | bodyDeclScopeName `isPrefixOf` x = iterDeclScopeName ++ drop (length bodyDeclScopeName) x
                | otherwise = x

            fixScopedName s@(ScopedNameF x (Just y) z)
                | getQualifiedName y == bodyDeclScopeName = ScopedNameF x iterScopedQualifier z
                | getQualifiedName s == loopInductionName = ScopedNameF x iterScopedQualifier z
              where
                iterScopedQualifier = scopedQualifier x y iterDeclScopeName
            fixScopedName x = x

            replicateLambda l@LambdaF{} = fmap (desugar (fixLambdaDeclScopeName . fixLambdaIdent)) l
              where
                fixLambdaName x
                    | x == getName l = suffixName iter x
                    | otherwise      = x

                fixLambdaIdent (IdentifierF x) = IdentifierF $ fixLambdaName x
                fixLambdaIdent x = x

                fixLambdaDeclScopeName = modifyDeclScopeName $ map fixLambdaName
            replicateLambda x = x

    eval _ = removeNestedSeq e
{-# INLINE staticControl #-}

evaluateConstexpr :: SymbolMap (Typed n) (ParseError s e) -> Algebra (NotedExpF (Typed n) (ParseError s e)) (NotedExp (Typed n) (ParseError s e))
evaluateConstexpr symbols (Note n e) = NotedExp n $ eval e
  where
    showUnmangled = show . unmangle symbols

    eval (TemplateInstanceF x y) = TemplateInstanceF x $ desugar' rewrite y
      where
        tIsConstAggregate t = tIsConst t && (tIsStruct t || tIsArray t || tIsUnion t)

        rewrite n'
            | tIsClosure $ theType n' = id
            | typeResolved n' = resolve
        rewrite _             = id

        resolve e'@(NamedValueF a)
            | tIsFunction $ typeOf a = unNote $ unfix a
            | tIsConstAggregate $ typeOf a =
                case lookupSymbol e' symbols of
                    Just (Variable _ _ Const{} _ (Just v) _)
                        | InitializerList{} <- v -> unNote $ unfix v
                    _                            -> e'
        resolve e'@TypeIdentifierF{} = case lookupSymbol e' symbols of
            Just (Alias _ t _)              -> unNote $ unfix t
            _                               -> e'
        resolve e' = e'

    eval (ArrayF x y z)
        | any isEnum $ unfix z = ArrayF x y $ renote (fmap resolveDim) z
      where
        resolveDim e' = maybe e' (flip newNote e' . untypedIntLiteral) $ fromNaturalConstexpr e'

    eval (ScopedNameF a (Just b) c)
        | tIsClass (theType b) || tIsEnum (theType b) = ScopedNameF a (scopedQualifier a c $ tQualifiedName $ theType b) c

    eval (BitOffsetF x (Identifier y))
        | typeResolved x = maybe (illegalOffsetOf "bitoffsetof" x y) (untypedIntLiteral . fromIntegral) $ offsetOf y x

    eval (ByteOffsetF x (Identifier y))
        | typeResolved x = maybe (illegalOffsetOf "byteoffsetof" x y) (bytesFromBits "byteoffsetof is undefined for fields with bit offset of ") $ offsetOf y x

    eval (UnaryF Clog2 x) =
        maybe e (untypedIntLiteral . fromIntegral . clog2) $ fromNaturalConstexpr x

    eval (BinaryF op a b)
        | typeResolved a && typeResolved b = evalBinary a b op
      where
        evalBinary (EnumValue _ x) y = evalBinary x y
        evalBinary x (EnumValue _ y) = evalBinary x y
        evalBinary (IntLiteral wx x) (IntLiteral wy y) = \case
            Add           -> intLiteral $ x + y
            Sub           -> intLiteral $ x - y
            Mul           -> intLiteral $ x * y
            LutMul        -> intLiteral $ x * y
            Div
              | y /= 0    -> intLiteral $ x `quot` y
              | otherwise -> desugarError "Division by zero is undefined"
            Mod
              | y /= 0    -> intLiteral $ x `rem` y
              | otherwise -> desugarError "Remainder by zero is undefined"
            BitwiseAnd    -> intLiteral $ x .&. y
            BitwiseOr     -> intLiteral $ x .|. y
            BitwiseXor    -> intLiteral $ x `xor` y
            ShiftLeft     -> intLiteral $ x `shiftL` fromIntegral y
            ShiftRight    -> intLiteral $ x `shiftR` fromIntegral y
            Equal         -> BoolLiteralF $ x == y
            NotEqual      -> BoolLiteralF $ x /= y
            Greater       -> BoolLiteralF $ x > y
            GreaterEqual  -> BoolLiteralF $ x >= y
            Less          -> BoolLiteralF $ x < y
            LessEqual     -> BoolLiteralF $ x <= y
            _             -> e
          where
            intLiteral
                | isNothing wx && isNothing wy = untypedIntLiteral
                | otherwise                      = typedIntLiteral $ inferBinaryOp op a b

        evalBinary (BoolLiteral x) (BoolLiteral y) = \case
            Equal         -> BoolLiteralF $ x == y
            NotEqual      -> BoolLiteralF $ x /= y
            Or            -> BoolLiteralF $ x || y
            And           -> BoolLiteralF $ x && y
            Xor           -> BoolLiteralF $ (x || y) && not (x && y)
            _             -> e

        evalBinary (StringLiteral x) (StringLiteral y) = \case
            Equal         -> BoolLiteralF $ x == y
            NotEqual      -> BoolLiteralF $ x /= y
            _             -> e

        evalBinary x y@(IntLiteral _ y')  = evalBinaryInt x y y'
        evalBinary x@(IntLiteral _ x') y  = evalBinaryInt y x x'

        evalBinary x (BoolLiteral y)    = evalBinaryBool x y
        evalBinary (BoolLiteral x) y    = evalBinaryBool y x

        evalBinary x y
            | tIsType (typeOf x) &&
              tIsType (typeOf y)        = evalBinaryType (theType x) (theType y)

        evalBinary _ _ = const e

        evalBinaryInt x _ _ _
            | not $ tIsInt $ typeOf x   = e
        evalBinaryInt x y 0 BitwiseAnd
            | sizeOf x <= sizeOf y      = untypedIntLiteral 0
        evalBinaryInt x _ y BitwiseOr
            | all (testBit y) $ take (fromJust $ sizeOf x) [0..]
                                        = untypedIntLiteral y
        evalBinaryInt _ _ _ _           = e

        evalBinaryBool x _ _
            | not $ tIsBool $ typeOf x  = e
        evalBinaryBool _ True Or        = BoolLiteralF True
        evalBinaryBool _ False And      = BoolLiteralF False
        evalBinaryBool _ _ _            = e

        evalBinaryType s t Equal        = BoolLiteralF (s <: t && t <: s)
        evalBinaryType s t NotEqual     = BoolLiteralF (not (s <: t) || not (t <: s))
        evalBinaryType s t LessEqual    = BoolLiteralF (s <: t)
        evalBinaryType s t Less         = BoolLiteralF (s <: t && not (t <: s))
        evalBinaryType s t GreaterEqual = BoolLiteralF (t <: s)
        evalBinaryType s t Greater      = BoolLiteralF (t <: s && not (s <: t))
        evalBinaryType _ _ _            = e

    eval (UnaryF op a) = fromMaybe e $ evalUnary op a
      where
        evalUnary Neg (FloatLiteral x) =
            Just $ FloatLiteralF (-x)

        evalUnary Neg x =
            intLiteral . negate <$> fromIntegralConstexpr x

        evalUnary Invert x
            | not $ typeResolved n  = Nothing
            | tIsEnum $ typeOf x    = invert (tEnumBase $ typeOf x)
            | isUntypedIntLiteral x = invert (tSigned $ typeOf x)
            | tIsInt $ typeOf x     = invert (typeOf x)
          where
            invert t
                | tIsUnsigned t = intLiteral . truncateUInt (fromJust $ tWidth t) . complement <$> fromIntegralConstexpr x
                | tIsSigned t   = intLiteral . truncateInt (fromJust $ tWidth t) . complement <$> fromIntegralConstexpr x
                | otherwise     = Nothing

        evalUnary BitSizeof x =
            untypedIntLiteral . fromIntegral <$> sizeOf x

        evalUnary ByteSizeof x =
            bytesFromBits "bytesizeof is undefined for a type with bit size of " <$> sizeOf x

        evalUnary Not (BoolLiteral x) =
            Just $ BoolLiteralF $ not x

        evalUnary _ _ = Nothing

        intLiteral
            | isUntypedIntLiteral a = untypedIntLiteral
            | otherwise             = typedIntLiteral $ inferUnaryOp op a

    eval (NamedValueF x)
        | tIsConst $ typeOf x = case lookupSymbol x symbols of
            Just (Variable _ _ (Const typ) _ (Just v) _)
                | (tIsInt (theType typ) || tIsFloat (theType typ)) && isIntLiteral v -> fromMaybe e $ castIntegralConstexpr (theType typ) =<< fromIntegralConstexpr v
                | isLiteral v || isEnum v || tIsFunction (typeOf v) -> unNote $ unfix v
            Just (EnumConstant _ _ (Just v))
                | isIntLiteral v -> EnumValueF x (NotedExp n $ unNote $ unfix v)
            _ -> e

    eval _
        | not $ typeResolved n = e

    eval (ParamIntF x) =
        maybe e IntegerF $ fromNaturalConstexpr x

    eval (ParamUintF x) =
        maybe e UnsignedF $ fromNaturalConstexpr x

    eval (DecltypeF _ x) =
        fromMaybe (illegalDecltype x) $ expFromType x $ typeOf x

    eval (CastF t x)
        | (Just a') <- fromIntegralConstexpr x = fromMaybe e $ castIntegralConstexpr (theType t) a'
        | (FloatLiteral f) <- x                = fromMaybe e $ castFloatConstexpr (theType t) f

    eval (MuxF (BoolLiteral x) y) =
        mux y $ fromEnum x

    eval (MuxF x y) =
        maybe e (mux y) $ fromIntegralConstexpr x

    eval (StaticF x)
        | isLiteral x = unNote $ unfix x

    eval _ = removeNestedSeq e

    untypedIntLiteral = IntLiteralF Nothing

    typedIntLiteral t = IntLiteralF $ Just $ LiteralType signedness width
      where
        signedness = if tIsSigned t then SignedLiteral else UnsignedLiteral
        width = fromJust $ tWidth t

    castIntegralConstexpr t
        | tIsSigned t   = Just . typedIntLiteral t . truncateInt width
        | tIsUnsigned t = Just . typedIntLiteral t . truncateUInt width
        | tIsFloat t    = Just . FloatLiteralF . castWord32ToFloat . fromIntegral
        | otherwise     = const Nothing
      where
        width = fromJust $ tWidth t

    castFloatConstexpr t
        | tIsSigned t   = Just . typedIntLiteral t . truncateInt width . fromIntegral . castFloatToWord32
        | tIsUnsigned t = Just . typedIntLiteral t . truncateUInt width . fromIntegral . castFloatToWord32
        | tIsFloat t    = Just . FloatLiteralF
        | otherwise     = const Nothing
      where
        width = fromJust $ tWidth t

    illegalDecltype t = desugarError $ "Argument of type `" ++ showUnmangled (typeOf t) ++ "` is illegal for `decltype`"

    illegalOffsetOf s x y = desugarError $ "Illegal operands for " ++ s ++ ": " ++ showUnmangled (theType x) ++ ", " ++ showPretty y

    bytesFromBits s x
        | x .&. 7 == 0 = untypedIntLiteral . fromIntegral $ x `shiftR` 3
        | otherwise    = desugarError $ s ++ show x

    mux x y = maybe err (unNote . unfix) $ maybeAt y $ toList $ unfix x
      where
        err = desugarError $ "Invalid selector `" ++ show y ++ "` for the `mux` with " ++ show (length $ unfix x) ++ " elements"
{-# INLINE evaluateConstexpr #-}

removeNestedScope :: NotedExp (Typed n) e -> NotedExp (Typed n) e
removeNestedScope x@(NestedScope _ _ y) = desugar rewrite y
  where
    nestedScopeQualifiedName = getQualifiedName x
    n = length nestedScopeQualifiedName
    rewrite e@(ScopedNameF a (Just b) c)
        | getQualifiedName e == nestedScopeQualifiedName = unNote $ unfix b
        | getQualifiedName b == nestedScopeQualifiedName = ScopedNameF a Nothing c
    rewrite e                                            = modifyDeclScopeName removeNestedScopeName e
    removeNestedScopeName s
        | nestedScopeQualifiedName `isPrefixOf` s = take (n - 1) s ++ drop n s
        | otherwise = s
removeNestedScope x = x

isEnum :: NotedExp n e -> Bool
isEnum = \case
    EnumValue _ _   -> True
    _               -> False

isLiteral :: NotedExp n e -> Bool
isLiteral = \case
    IntLiteral{}    -> True
    BoolLiteral{}   -> True
    FloatLiteral{}  -> True
    StringLiteral{} -> True
    _               -> False

constexprResolved :: NotedExp n e -> Bool
constexprResolved = \case
    InitializerList e -> all constexprResolved $ unfix e
    Designator _ e    -> constexprResolved e
    Positional _ e    -> constexprResolved e
    e                 -> isLiteral e || isEnum e

