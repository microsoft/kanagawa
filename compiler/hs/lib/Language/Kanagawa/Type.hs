{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE TupleSections #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Type
  ( Type
  , TFuncParam(..)
  , TAttr(..)
  , TArg(..)
  , Typed(untype)
  , TypedExp(..)
  , intrinsics
  , inferExpType
  , inferBinaryOp
  , inferUnaryOp
  , inferIntLiteralType
  , deduceTemplateArgs
  , deduceAuto
  , expFromType
  , expFromTAttr
  , isIntLiteral
  , isUntypedIntLiteral
  , fromIntegralConstexpr
  , fromNaturalConstexpr
  , noteType
  , unnoteType
  , newNote
  , localIdentifier
  , thisNamedValue
  , qualifiedIdentifier
  , prettyTypedExp
  , tResolved
  , tSigned
  , tUnConst
  , tWidth
  , tElement
  , tEnumBase
  , tInitializedType
  , tInitializerType
  , tQualifiedName
  , tInstanceTemplate
  , tInstanceArgs
  , tClosureFunction
  , tClosureCapture
  , tFunctionAttr
  , tFunctionParams
  , tReturn
  , tIsUndefined
  , tIsError
  , tIsVoid
  , tIsConst
  , tIsClass
  , tIsInstance
  , tIsStruct
  , tIsUnion
  , tIsFunction
  , tIsMethod
  , tIsEccMem
  , tIsArray
  , tIsArrayOf
  , tIsBool
  , tIsEnum
  , tIsSigned
  , tIsUnsigned
  , tIsInt
  , tIsFloat
  , tIsType
  , tIsReference
  , tIsTemplate
  , tIsClosure
  , tIsDesignatedInitializer
  , tIsValueType
  , setTypeOfInstance
  , showPretty
  , (<:)
  , tIsConvertibleTo
  , data'closure
  , data'closure'core
  , data'closure'core'Closure
  , unmangleInstanceName
  , unmangle
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits (shiftL)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Classes
import Data.List
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Prettyprinter
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Internal
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.PrettyPrint()
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols

data'closure :: Name
data'closure = moduleNamespace ["data", "closure"]

data'closure'core :: Name
data'closure'core = moduleNamespace ["data", "closure", "core"]

data'closure'core'Closure :: QualifiedName
data'closure'core'Closure = [data'closure'core, "Closure"]

data TFuncParam =
    TFuncParam
    { attrOfParam :: Set TAttr
    , typeOfParam :: Type
    , nameOfParam :: Maybe Name
    }
    deriving (Ord, Show)

instance Eq TFuncParam where
    (==) a b = typeOfParam a == typeOfParam b &&
               attrOfParam a == attrOfParam b

class Unmangle a where
    unmangle :: (Pretty a) =>  SymbolMap (Typed n) e -> a -> Doc ann
    unmangle _                  = pretty

instance Unmangle TArg where
    unmangle s (TTypeArg a)     = unmangle s a
    unmangle s (TNameArg _ a)   = unmangleQualifiedName s a
    unmangle _ x                = pretty x

instance Unmangle TFuncParam where
    unmangle s a = prettyAttr (attrOfParam a) <> unmangle s (typeOfParam a)

instance Unmangle Type where
    unmangle s (TType t)             = "type" <+> unmangle s t
    unmangle s (TConst t)            = "const" <+> unmangle s t
    unmangle s (TReference x)        = unmangleQualifiedName s x <> "&"
    unmangle s (TArray as t ns)      = formatArray (unmangle s) as t ns
    unmangle s (TFunction _ as t ts) = prettyAttr as <> parens (hsep . punctuate comma $ map (unmangle s) ts) <+> "->" <+> unmangle s t
    unmangle s (TEnum x _)           = unmangleQualifiedName s x
    unmangle s (TStruct x _)         = unmangleQualifiedName s x
    unmangle s (TUnion x _)          = unmangleQualifiedName s x
    unmangle s (TClass x _)          = unmangleQualifiedName s x
    unmangle s i@(TInstance t x _)
        | tIsFunction t              = unmangle s t
        | tIsError t                 = pretty t
        | not $ tResolved t          = pretty t
        | null (init x)              = unmangleInstanceName s i
        | otherwise                  = unmangleQualifiedName s (init x) <> "::" <> unmangleInstanceName s i
    unmangle _ t                     = pretty t

formatArray :: (Type -> Doc ann) -> Set TAttr -> Type -> [Integer] -> Doc ann
formatArray fn as (TArray as' t' ns') ns
    | null as           = formatArray fn as' t' (ns ++ ns')
    | null (init ns)    = prettyAttr as <> parens (formatArray fn as' t' ns') <> prettyDimensions ns
    | otherwise         = parens (prettyAttr as <> parens (formatArray fn as' t' ns') <> prettyDimensions [last ns]) <> prettyDimensions (init ns)
formatArray fn as t ns
    | null as           = fn t <> prettyDimensions ns
    | null (init ns)    = prettyAttr as <> fn t <> prettyDimensions ns
    | otherwise         = parens (prettyAttr as <> fn t <> prettyDimensions [last ns]) <> prettyDimensions (init ns)

unmangleInstanceName :: SymbolMap (Typed n) e -> Type -> Doc ann
unmangleInstanceName s (TInstance _ x as) = pretty (last x) <> angles (hsep $ punctuate comma $ map (unmangle s . snd) as)
unmangleInstanceName _ _ = undefined

unmangleQualifiedName :: SymbolMap (Typed n) e -> QualifiedName -> Doc ann
unmangleQualifiedName symbols = hcat . punctuate "::" . map unmangleName . tail . inits
  where
    unmangleName x = case lookupSymbolByName x symbols of
        Just e
            | tIsInstance $ theType e -> unmangleInstanceName symbols $ theType e
            | tIsInstance $ typeOf e  -> unmangleInstanceName symbols $ typeOf e
        _                             -> pretty $ unmangleModuleNamespace $ last x

instance Pretty TFuncParam where
    pretty a = prettyAttr (attrOfParam a) <> pretty (typeOfParam a)

data TAttr =
    TFlagAttr Flag
    |
    TIntAttr Attribute Integer
    |
    TAttr Attribute QualifiedName Type
    deriving (Eq, Ord, Show)

instance Pretty TAttr where
    pretty (TFlagAttr a) = pretty a
    pretty (TIntAttr a b) = pretty a <> parens (pretty b)
    pretty (TAttr a b _) = pretty a <> parens (pretty $ intercalate "::" $ map toString b)

data TArg =
    TArg String
    |
    TStrArg String
    |
    TTypeArg Type
    |
    TIntArg Integer
    |
    TNameArg Type QualifiedName
    deriving (Eq, Ord, Show)

instance Pretty TArg where
    pretty (TArg a) = pretty a
    pretty (TStrArg a) = pretty a
    pretty (TTypeArg a) = pretty a
    pretty (TIntArg a) = pretty a
    pretty (TNameArg _ a) = pretty $ intercalate "::" $ map toString a

data Type =
    TAuto
    |
    TTemplate
    |
    TType Type
    |
    TUndefined
    |
    TUntyped
    |
    TUnresolved
    |
    TInitializer [Type]
    |
    TDesignator Name Type
    |
    TPositional Int Type
    |
    TVoid
    |
    TBoolean
    |
    TFloat
    |
    TString
    |
    TSigned Int
    |
    TUnsigned Int
    |
    TDependent Type
    |
    TConst Type
    |
    TReference QualifiedName
    |
    TArray (Set TAttr) Type [Integer]
    |
    TFunction FunctionKind (Set TAttr) Type [TFuncParam]
    |
    TClosure QualifiedName Type Type QualifiedName
    |
    TEnum QualifiedName Type
    |
    TStruct QualifiedName [(Name, Type)]
    |
    TUnion QualifiedName [(Name, Type)]
    |
    TClass QualifiedName [(Name, Type)]
    |
    TInstance Type QualifiedName [(Name, TArg)]
    |
    TError String
    deriving (Eq, Ord, Show)

tFieldAt :: Type -> Int -> Type
tFieldAt t x = fromMaybe (TError $ "Excess element in initializer of type " ++ showPretty t) $
    tLookupField ((snd <$>) .: maybeAt) t x

tPositionalType :: Type -> Type
tPositionalType (TPositional _ t) = t
tPositionalType _ = TUnresolved

tInitializerType :: Type -> Type
tInitializerType (TDesignator _ t) = t
tInitializerType (TPositional _ t) = t
tInitializerType _ = undefined

tInitializedType :: Type -> Type -> Type
tInitializedType (TConst t) x                 = tInitializedType t x
tInitializedType t (TPositional x _)
    | tIsArray t && x == tDimension t         = TError "Excess elements in array initializer"
    | tIsUnion t && x /= 0                    = TError "Excess elements in union initializer"
    | tIsArray t                              = tElement t
    | tIsUnion t || tIsStruct t || tIsClass t = tFieldAt t x
    | otherwise                               = TError $ "Non-empty initializer list cannot initialize type " ++ showPretty t
tInitializedType t (TDesignator x _)
    | tIsUnion t || tIsStruct t || tIsClass t = tField t x
    | otherwise                               = TError $ "Member designator cannot initialize type " ++ showPretty t
tInitializedType _ _                          = undefined

tAggregateInitializer :: (Type -> Type -> Bool) -> Type -> Type -> Bool
tAggregateInitializer _ (TInitializer ts) t
    | tIsUnion t && length ts > 1             = False
tAggregateInitializer fn (TInitializer ts) t
    |  tIsUnion t
    || tIsStruct t
    || tIsClass t
    || tIsArray t                             = all (liftM2 fn tInitializerType (tInitializedType t)) ts
tAggregateInitializer _ _ _                   = False

-- Is a subtype of
(<:) :: Type -> Type -> Bool
(<:) TError{} _                     = False
(<:) _ TError{}                     = False
(<:) s t | s == t                   = True
(<:) (TUnsigned x) (TUnsigned y)    = x < y
(<:) (TUnsigned x) (TSigned y)      = x < y
(<:) (TSigned x) (TSigned y)        = x < y
(<:) (TConst s) t                   = s <: t
(<:) s (TConst t)                   = s <: t
(<:) s@TInstance{} t                = tConcrete s <: t
(<:) s t@TInstance{}                = s <: tConcrete t
(<:) (TEnum _ s) t                  = s <: t
(<:) s (TDependent t)               = s <: t
(<:) (TFunction _ a1 r1 p1)
     (TFunction _ a2 r2 p2)         = (r2 == TAuto || r1 <: r2)
                                      && liftEq ((<:) `on` typeOfParam) p2 p1
                                      && liftEq (Set.isSubsetOf `on` attrOfParam) p2 p1
                                      && (a2 `Set.isSubsetOf` a1)
(<:) (TInitializer []) TBoolean     = True
(<:) (TInitializer []) TFloat       = True
(<:) (TInitializer []) t | tIsInt t = True
(<:) (TInitializer []) t | tIsEnum t = True
(<:) s@TInitializer{} t             = tAggregateInitializer (<:) s t
(<:) (TClosure n1 f1 s _)
     (TClosure n2 f2 t _)           = n1 == n2
                                      && f1 <: f2
                                      && s <: t
(<:) s@(TArray a1 _ ds)
     t@(TArray a2 _ (d2:_))         = a1 == a2
                                      && (null ds || head ds == d2)
                                      && tElement s <: tElement t
                                      && tElement t <: tElement s
(<:) TAuto _                        = True
(<:) _ _                            = False

infixr 4 <:

tIsConvertibleTo :: Type -> Type -> Bool
tIsConvertibleTo s t
    | s <: t                        = True
tIsConvertibleTo (TConst s) t       = tIsConvertibleTo s t
tIsConvertibleTo s (TConst t)       = tIsConvertibleTo s t
tIsConvertibleTo s@TInstance{} t    = tIsConvertibleTo (tConcrete s) t
tIsConvertibleTo s t@TInstance{}    = tIsConvertibleTo s (tConcrete t)
tIsConvertibleTo s@TInitializer{} t = tAggregateInitializer tIsConvertibleTo s t
tIsConvertibleTo (TEnum _ s) t      = tIsConvertibleTo s t
tIsConvertibleTo s (TDependent t)   = tIsConvertibleTo s t
tIsConvertibleTo s t
    | tIsInt s && tIsInt t          = True
tIsConvertibleTo _ _                = False

commaSep :: [TAttr] -> Doc ann
commaSep = catBy ","

prettyAttr :: Set TAttr -> Doc ann
prettyAttr xs
    | Set.null xs = mempty
    | otherwise   = brackets (brackets $ commaSep $ Set.toList xs) <> space

instance Pretty Type where
    pretty TAuto                 = "auto"
    pretty (TType a)             = "type" <+> pretty a
    pretty TTemplate             = "template"
    pretty TUntyped              = ""
    pretty TUndefined            = "undefined"
    pretty TUnresolved           = "unresolved"
    pretty (TInitializer ts)     = braces (sepBy comma ts)
    pretty TVoid                 = "void"
    pretty TBoolean              = "bool"
    pretty (TDesignator x t)     = "." <> pretty x <+> "=" <+> pretty t
    pretty (TPositional _ t)     = pretty t
    pretty TFloat                = "float"
    pretty TString               = "string"
    pretty (TSigned n)           = "int" <> pretty n
    pretty (TUnsigned n)         = "uint" <> pretty n
    pretty (TDependent t)        = pretty t
    pretty (TConst t)            = "const" <+> pretty t
    pretty (TReference x)        = prettyQualifiedName x <> "&"
    pretty (TArray as t ns)      = formatArray pretty as t ns
    pretty (TFunction _ as t ts) = prettyAttr as <> parens (sepBy comma ts) <+> "->" <+> pretty t
    pretty (TClosure x _ c _)    = brackets (pretty c) <+> prettyQualifiedName x
    pretty (TEnum x _)           = prettyQualifiedName x
    pretty (TStruct x _)         = prettyQualifiedName x
    pretty (TUnion x _)          = prettyQualifiedName x
    pretty (TClass x _)          = prettyQualifiedName x
    pretty (TInstance t x as)
        | tIsFunction t          = pretty t
        | tIsError t             = pretty t
        | not $ tResolved t      = pretty t
        | otherwise              = prettyQualifiedName x <> angles (sepBy comma $ map snd as)
    pretty (TError e)            = pretty e

prettyDimensions :: [Integer] -> Doc ann
prettyDimensions xs
    | null xs    = brackets mempty
    | otherwise  = foldMap (brackets . pretty) xs

prettyQualifiedName :: QualifiedName -> Doc ann
prettyQualifiedName (x:xs) = catBy "::" $ unmangleModuleNamespace x : xs
prettyQualifiedName [] = mempty

prettyTypedExp :: Show e => NotedExp (Typed n) e -> Doc ann
prettyTypedExp = cata typed
  where
    typed x@(Note _ e) = nest 4 $ line <> parens (pretty $ typeOf x) <+> colon <+> viaShow e

showPretty :: Pretty a => a -> String
showPretty = show . pretty

setTypeOfInstance ::
    QualifiedName ->
    [(Name, NotedExp (Typed n) e)] ->
    NotedExp (Typed n) e ->
    NotedExp (Typed n) e
setTypeOfInstance s = setTypeOfExp . TInstance TUnresolved s . map (second tArg)
  where
    second f (x, y) = (x, f y)
    tArg (IntLiteral _ x) = TIntArg x
    tArg (StringLiteral x) = TStrArg x
    tArg e@QualifiedIdentifier{} = TNameArg (typeOf e) $ getQualifiedName e
    tArg e = case typeOf e of
        TType x -> TTypeArg x
        _       -> TArg $ showPretty e

sepBy, catBy :: Pretty a => Doc ann -> [a] -> Doc ann
sepBy x = hsep . punctuate x . map pretty
catBy x = hcat . punctuate x . map pretty

data Typed a =
     Typed
         { typeof :: !Type
         , untype :: a
         }
    deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Typed a) where
    x <> y = Typed (typeof x) (untype x <> untype y)

class TypedExp a where
    typeOf :: a -> Type

    theType :: a -> Type
    theType = tType . typeOf
      where
        tType (TType x) = x
        tType x
            | not $ tResolved x
                        = TUnresolved
        tType _         = TError "Expected a type"

    sizeOf :: a -> Maybe Int
    sizeOf = tWidth . typeOf

    offsetOf :: Name -> a -> Maybe Int
    offsetOf x = tOffsetOf x . theType

    typeResolved :: a -> Bool
    typeResolved = tResolved . typeOf

instance TypedExp (Typed n) where
    typeOf = typeof

instance TypedExp (NotedExpF (Typed n) e a) where
    typeOf = typeof . note

instance TypedExp (NotedExp (Typed n) e) where
    typeOf = typeof . note . unfix

inferIntLiteralType :: Integral a => Maybe LiteralType -> a -> Type
inferIntLiteralType (Just (LiteralType SignedLiteral w)) _ = TSigned w
inferIntLiteralType (Just (LiteralType UnsignedLiteral w)) _ = TUnsigned w
inferIntLiteralType Nothing x
    | x == 0                        = TUnsigned 1
    | x < 0                         = TSigned $ 1 + clog2 (abs (fromIntegral x))
    | otherwise                     = TUnsigned $ clog2 (fromIntegral x + 1)

tClosureFunction :: Type -> QualifiedName
tClosureFunction (TClosure x _ _ _) = x
tClosureFunction (TConst x)         = tClosureFunction x
tClosureFunction x@TInstance{}      = tClosureFunction $ tConcrete x
tClosureFunction _                  = undefined

tClosureCapture :: Type -> Type
tClosureCapture (TClosure _ _ x _)  = x
tClosureCapture (TConst x)          = tClosureCapture x
tClosureCapture x@TInstance{}       = tClosureCapture $ tConcrete x
tClosureCapture _                   = undefined

tQualifiedName :: Type -> QualifiedName
tQualifiedName (TClosure _ _ _ x)   = x
tQualifiedName (TClass x _)         = x
tQualifiedName (TStruct x _)        = x
tQualifiedName (TUnion x _)         = x
tQualifiedName (TEnum x _)          = x
tQualifiedName (TReference x)       = x
tQualifiedName (TInstance x _ _)    = tQualifiedName x
tQualifiedName (TConst x)           = tQualifiedName x
tQualifiedName _                    = undefined

tInstanceTemplate :: Type -> QualifiedName
tInstanceTemplate (TInstance _ x _) = x
tInstanceTemplate _                 = undefined

tInstanceArgs :: Type -> [(Name, TArg)]
tInstanceArgs (TInstance _ _ x)     = x
tInstanceArgs _                     = undefined

tFunctionAttr :: Type -> Set TAttr
tFunctionAttr (TFunction _ a _ _)   = a
tFunctionAttr (TInstance a _ _)     = tFunctionAttr a
tFunctionAttr (TConst a)            = tFunctionAttr a
tFunctionAttr _                     = undefined

tFunctionParams :: Type -> [TFuncParam]
tFunctionParams (TFunction _ _ _ a) = a
tFunctionParams (TClosure _ a _ _)  = tail $ tFunctionParams a
tFunctionParams (TInstance a _ _)   = tFunctionParams a
tFunctionParams (TConst a)          = tFunctionParams a
tFunctionParams _                   = undefined

tDimension :: Num a => Type -> a
tDimension t@TInstance{}            = tDimension $ tConcrete t
tDimension (TConst t)               = tDimension t
tDimension (TArray _ _ (d:_))       = fromIntegral d
tDimension _                        = undefined

tInvalidOperandsError :: Type -> Type -> Type
tInvalidOperandsError x y   = TError $ "Invalid operands to binary expression: `" ++ showPretty x ++ "` and `" ++ showPretty y ++ "`"

tExpectedIntegerError :: Type -> Type
tExpectedIntegerError x = TError $ "Invalid operand, expected an integer: `" ++ showPretty x  ++ "`"

tSigned, tInvert, tNeg, tNot, tReturn, tConst, tUnConst, tConcrete, tEnumBase :: Type -> Type
tSigned x@TSigned{}         = x
tSigned (TUnsigned x)       = TSigned (x + 1)
tSigned x
    | not $ tResolved x     = TUnresolved
tSigned _                   = TError "Expected an integer"

tEnumBase (TEnum _ x)       = x
tEnumBase (TConst x)        = tEnumBase x
tEnumBase (TInstance x _ _) = tEnumBase x
tEnumBase _                 = TError "Expected an enum"

tInvert x
    | tIsInt x              = x
tInvert x                   = tExpectedIntegerError x

tNeg TFloat                 = TFloat
tNeg (TSigned x)            = TSigned (x + 1)
tNeg (TUnsigned x)          = TSigned (x + 1)
tNeg _                      = TError "Expected an integer or a float operand"

tNot TBoolean               = TBoolean
tNot _                      = TError "Expected a boolean operand"

tReturn (TFunction _ _ x _)
    | x == TAuto            = TUnresolved
    | otherwise             = x
tReturn (TClosure _ x _ _)  = tReturn x
tReturn (TInstance x _ _)   = tReturn x
tReturn (TConst x)          = tReturn x
tReturn x
    | not $ tResolved x     = TUnresolved
tReturn x@TError{}          = x
tReturn _                   = TError "Undefined function"

tConst x@TConst{}           = x
tConst x@TError{}           = x
tConst (TType x)            = TType $ tConst x
tConst x
    | not $ tResolved x     = TUnresolved
tConst x                    = TConst x

tUnConst (TConst x)         = x
tUnConst (TType x)          = TType $ tUnConst x
tUnConst (TInstance x y z)  = TInstance (tUnConst x) y z
tUnConst x                  = x

tConcrete (TInstance x _ _) = tConcrete x
tConcrete x                 = x

tInner :: Type -> Maybe Type
tInner (TConst x)           = Just x
tInner (TInstance x _ _)    = Just x
tInner _                    = Nothing

maybeInner :: (Type -> Bool) -> Type -> Bool
maybeInner f = maybe False f . tInner

tIsUndefined, tIsType, tIsReference, tIsTemplate, tIsClosure, tIsError, tIsVoid, tIsConst, tIsFloat, tIsInt, tIsClass, tIsStruct, tIsUnion, tIsBool, tIsEnum, tIsSigned, tIsUnsigned, tIsEccMem, tIsArray, tIsFunction, tIsMethod, tIsInstance, tIsDesignator, tIsDesignatedInitializer, tIsValueType, tIsString :: Type -> Bool
tIsUndefined TUndefined     = True
tIsUndefined _              = False

tIsConst TConst{}           = True
tIsConst _                  = False

tIsTemplate TTemplate       = True
tIsTemplate _               = False

tIsClosure (TInstance _ x _) = x == data'closure'core'Closure
tIsClosure x                 = maybeInner tIsClosure x

tIsType TType{}             = True
tIsType x                   = maybeInner tIsType x

tIsReference TReference{}   = True
tIsReference x              = maybeInner tIsReference x

tIsError TError{}           = True
tIsError x                  = maybeInner tIsError x

tIsVoid TVoid               = True
tIsVoid x                   = maybeInner tIsVoid x

tIsString TString           = True
tIsString x                 = maybeInner tIsString x

tIsFloat TFloat{}           = True
tIsFloat x                  = maybeInner tIsFloat x

tIsClass TClass{}           = True
tIsClass x                  = maybeInner tIsClass x

tIsStruct TStruct{}         = True
tIsStruct x                 = maybeInner tIsStruct x

tIsUnion TUnion{}           = True
tIsUnion x                  = maybeInner tIsUnion x

tIsBool TBoolean{}          = True
tIsBool x                   = maybeInner tIsBool x

tIsEnum TEnum{}             = True
tIsEnum x                   = maybeInner tIsEnum x

tIsSigned TSigned{}         = True
tIsSigned x                 = maybeInner tIsSigned x

tIsUnsigned TUnsigned{}     = True
tIsUnsigned x               = maybeInner tIsUnsigned x

tIsFunction TFunction{}     = True
tIsFunction x               = maybeInner tIsFunction x

tIsMethod (TFunction x _ _ _) = x == MemberFunction
tIsMethod x                 = maybeInner tIsMethod x

tIsInstance TInstance{}     = True
tIsInstance _               = False

tIsInt x                    = tIsUnsigned x || tIsSigned x

tIsEccMem (TArray x _ _)    = any eccAttr x
  where
    eccAttr (TAttr Ecc _ _)
              = True
    eccAttr _ = False
tIsEccMem x                 = maybeInner tIsEccMem x

tIsArray                    = tIsArrayOf (const True)

tIsDesignator TDesignator{} = True
tIsDesignator _             = False

tIsDesignatedInitializer (TInitializer ts)
                            = all tIsDesignator ts
tIsDesignatedInitializer _  = False

tIsValueType x = tIsClosure x || tIsBool x || tIsInt x || tIsFloat x || tIsEnum x || tIsString x || tIsStruct x || tIsUnion x || tIsArrayOf tIsValueType x

tIsArrayOf :: (Type -> Bool) -> Type ->Bool
tIsArrayOf f (TArray _ x _) = f x || tIsArrayOf f x
tIsArrayOf f x              = maybeInner (tIsArrayOf f) x

tSumWidths :: [(Name, Type)] -> Maybe Int
tSumWidths = foldr (liftM2 (+) . tWidth . snd) (Just 0)

tWidth :: Type -> Maybe Int
tWidth TAuto              = Nothing
tWidth TTemplate          = Nothing
tWidth (TType x)          = tWidth x
tWidth TInitializer{}     = Nothing
tWidth TVoid              = Nothing
tWidth TBoolean           = Just 1
tWidth (TDesignator _ x)  = tWidth x
tWidth (TPositional _ x)  = tWidth x
tWidth TFloat             = Just 32
tWidth TString            = Just 0
tWidth (TSigned x)        = Just x
tWidth (TUnsigned x)      = Just x
tWidth TDependent{}       = Nothing
tWidth (TConst x)         = tWidth x
tWidth TReference{}       = Just 1
tWidth (TArray _ _ [])    = Nothing
tWidth (TArray _ x xs)    = (*) (fromIntegral $ product xs) <$> tWidth x
tWidth TFunction{}        = Nothing
tWidth (TClosure _ _ x _) = tWidth x
tWidth (TEnum _ x)        = tWidth x
tWidth (TStruct _ x)      = tSumWidths x
tWidth (TUnion _ x)       = maximum $ fmap (tWidth . snd) x
tWidth TClass{}           = Nothing
tWidth (TInstance x _ _)  = tWidth x
tWidth TError{}           = Nothing
tWidth TUndefined         = Nothing
tWidth TUnresolved        = Nothing
tWidth TUntyped           = Nothing

tOffsetOf :: Name -> Type -> Maybe Int
tOffsetOf field x@TInstance{} = tOffsetOf field $ tConcrete x
tOffsetOf field (TStruct _ x) = case findIndex ((field ==) . fst) x of
                                 Just n -> tSumWidths $ take n x
                                 Nothing -> Nothing
tOffsetOf _ _ = Nothing

typeOfOperand :: NotedExp (Typed n) e -> Type
typeOfOperand = go . typeOf
  where
    go (TEnum _ x)        = go x
    go (TInstance x _ _ ) = go x
    go (TConst x)         = go x
    go x                  = x

inferUnaryOp :: UnaryOp -> NotedExp (Typed n) e -> Type
inferUnaryOp op x = case op of
    Invert
      | isUntypedIntLiteral x -> tInvert $ tSigned $ typeOfOperand x
      | otherwise             -> tInvert $ typeOfOperand x
    Neg
      | isIntLiteral x        -> tSuper (typeOfOperand x) (inferIntLiteralType Nothing $ negateInteger x)
      | otherwise             -> tNeg $ typeOfOperand x
    Not                       -> tNot $ typeOfOperand x
    BitSizeof                 -> TUnresolved
    ByteSizeof                -> TUnresolved
    Clog2                     -> TUnresolved
  where
    negateInteger :: NotedExp n e -> Integer
    negateInteger = negate . fromJust . fromIntegralConstexpr

inferBinaryOp :: BinaryOp -> NotedExp (Typed n) e -> NotedExp (Typed n) e -> Type
inferBinaryOp = infer
  where
    infer BitwiseAnd a b
        | isUntypedIntLiteral b = tBinaryOpLiteral tAndLiteral a b
        | isUntypedIntLiteral a = tBinaryOpLiteral tAndLiteral b a
    infer Mul a b
        | isUntypedIntLiteral b = tBinaryOpLiteral tMulLiteral a b
        | isUntypedIntLiteral a = tBinaryOpLiteral tMulLiteral b a
    infer Div a b
        | isIntLiteral b        = tBinaryOpLiteral tDivLiteral a b
    infer Mod a b
        | isIntLiteral b        = tBinaryOpLiteral tModLiteral a b
    infer ShiftLeft a b
        | isIntLiteral b        = tBinaryOpLiteral tShiftLLiteral a b
    infer ShiftRight a b
        | isIntLiteral b        = tBinaryOpLiteral tShiftRLiteral a b
    infer op a b                = tBinaryOp op a b

    tBinaryOpLiteral fn x y = fn (typeOfOperand x) (fromJust $ fromIntegralConstexpr y)

    tBinaryOp op x y = result op (typeOfOperand x) (typeOfOperand y)
      where
        result Add          = tAdd
        result Sub          = tSub
        result Mul          = tMul
        result LutMul       = tMul
        result Div          = tDiv
        result Mod          = tMod
        result BitwiseAnd   = tBitwiseAnd
        result BitwiseOr    = tBitwiseOr
        result BitwiseXor   = tBitwiseOr
        result ShiftRight   = tShiftR
        result ShiftLeft    = tShiftL
        result Equal        = const $ const TBoolean
        result NotEqual     = const $ const TBoolean
        result Greater      = const $ const TBoolean
        result GreaterEqual = const $ const TBoolean
        result Less         = const $ const TBoolean
        result LessEqual    = const $ const TBoolean
        result Or           = tLogical
        result And          = tLogical
        result Xor          = tLogical

tWidenInt, tNarrowInt :: Type -> Int -> Type
tWidenInt (TUnsigned x) n            = TUnsigned $ x + n
tWidenInt (TSigned x) n              = TSigned $ x + n
tWidenInt x _                        = tExpectedIntegerError x

tNarrowInt (TUnsigned x) n           = TUnsigned $ max 1 $ x - n
tNarrowInt (TSigned x) n             = TSigned $ max 2 $ x - n
tNarrowInt x _                       = tExpectedIntegerError x

tModLiteral, tAndLiteral, tMulLiteral, tDivLiteral :: Type -> Integer -> Type
tModLiteral _ n
    | n == 0                         = TError "Remainder by zero is undefined"
    | n < 0                          = TError "Remainder with negative denominator is undefined"
tModLiteral (TUnsigned x) n
    | n == 1                         = TUnsigned 1
    | otherwise                      = TUnsigned $ min x (clog2 n)
tModLiteral (TSigned x) n
    | n == 1                         = TSigned 2
    | otherwise                      = TSigned $ min x (1 + clog2 n)
tModLiteral x _                      = tExpectedIntegerError x

tAndLiteral (TUnsigned x) n
    | n == 0                         = TUnsigned 1
    | n < 0                          = TUnsigned x
    | otherwise                      = TUnsigned $ min x $ clog2 (n + 1)
tAndLiteral (TSigned x) n
    | n == 0                         = TSigned 2
    | n < 0                          = TSigned $ max x $ 1 + clog2 (1 - n)
    | otherwise                      = TSigned $ 1 + clog2 (n + 1)
tAndLiteral x _                      = tExpectedIntegerError x

tMulLiteral x n
    | not $ tIsInt x                 = tExpectedIntegerError x
    | n == 0                         = if tIsSigned x then TSigned 2 else TUnsigned 1
    | n < 0                          = tMulLiteral (tNeg x) (abs n)
    | otherwise                      = tWidenInt x $ clog2 n

tDivLiteral x n
    | not $ tIsInt x                 = tExpectedIntegerError x
    | n == 0                         = TError "Division by zero is undefined"
    | n < 0                          = TError "Division with negative denominator is undefined"
    | otherwise                      = tNarrowInt x $ clog2 n

tShiftLLiteral, tShiftRLiteral :: Type -> Int -> Type
tShiftLLiteral x n
    | tIsInt x                       = tWidenInt x n
    | otherwise                      = tExpectedIntegerError x

tShiftRLiteral x n
    | tIsInt x                       = tNarrowInt x n
    | otherwise                      = tExpectedIntegerError x

tAdd, tSub, tMul, tDiv, tMod, tBitwiseOr, tBitwiseAnd, tLogical, tShiftR, tShiftL, tSuper :: Type -> Type -> Type

tAdd (TUnsigned x) (TUnsigned y)     = TUnsigned (1 + max x y)
tAdd (TSigned x) (TSigned y)         = TSigned (1 + max x y)
tAdd x@TUnsigned{} y@TSigned{}       = tAdd (tSigned x) y
tAdd x@TSigned{} y@TUnsigned{}       = tAdd x (tSigned y)
tAdd TFloat TFloat                   = TFloat
tAdd x y                             = tInvalidOperandsError x y

tSub (TUnsigned x) (TUnsigned y)     = TSigned (1 + max x y)
tSub x y                             = tAdd x y

tMul (TUnsigned x) (TUnsigned y)     = TUnsigned (x + y)
tMul (TSigned x) (TUnsigned y)       = TSigned (x + y)
tMul (TUnsigned x) (TSigned y)       = TSigned (x + y)
tMul (TSigned x) (TSigned y)         = TSigned (x + y)
tMul TFloat TFloat                   = TFloat
tMul x y                             = tInvalidOperandsError x y

tDiv x y | tIsInt x && tIsInt y      = x
tDiv x y                             = tInvalidOperandsError x y

tMod TUnsigned{} y | tIsInt y        = TUnsigned $ fromJust $ tWidth y
tMod TSigned{} y | tIsInt y          = TSigned $ fromJust $ tWidth y
tMod x y                             = tInvalidOperandsError x y

tBitwiseOr x y
    | tIsInt x && tIsInt y           = tSuper x y
tBitwiseOr x y                       = tInvalidOperandsError x y

tBitwiseAnd (TUnsigned x) (TUnsigned y)
                                     = TUnsigned $ min x y
tBitwiseAnd x y
    | tIsInt x && tIsInt y           = tSuper x y
tBitwiseAnd x y                      = tInvalidOperandsError x y

tLogical TBoolean TBoolean           = TBoolean
tLogical x y                         = tInvalidOperandsError x y

tShiftR x y | tIsInt x && tIsInt y   = x
tShiftR x y                          = tInvalidOperandsError x y

tShiftL x y
    | not (tIsInt x && tIsInt y)     = tInvalidOperandsError x y
    | fromJust (tWidth y) >= 20      = TError "Invalid shift left. Result type exceeds maximum bit width."
    | otherwise                      = tWidenInt x ((1 `shiftL` fromJust (tWidth y)) - 1)

tSuper x y
    | not $ tResolved x              = TUnresolved
    | not $ tResolved y              = TUnresolved
    | x == y                         = x
tSuper (TUnsigned x) (TUnsigned y)   = TUnsigned (max x y)
tSuper (TSigned x) (TSigned y)       = TSigned (max x y)
tSuper x@TSigned{} y@TUnsigned{}     = tSuper x (tSigned y)
tSuper x@TUnsigned{} y@TSigned{}     = tSuper (tSigned x) y
tSuper x@TEnum{} y                   = tSuper (tEnumBase x) y
tSuper x y@TEnum{}                   = tSuper x (tEnumBase y)
tSuper (TInstance x _ _) y           = tSuper x y
tSuper x (TInstance y _ _)           = tSuper x y
tSuper (TPositional l x) (TPositional r y)
    | l == r                         = TPositional l $ tSuper x y
tSuper (TDesignator l x) (TDesignator r y)
    | l == r                         = TDesignator l $ tSuper x y
tSuper x@TInitializer{} y
    | x `tIsConvertibleTo` y         = y
tSuper x y@TInitializer{}
    | y `tIsConvertibleTo` x         = x
tSuper (TInitializer xs)
       (TInitializer ys)
    | length xs >= length ys         = TInitializer (zipWith tSuper xs ys ++ drop (length ys) xs)
    | otherwise                      = TInitializer (zipWith tSuper xs ys ++ drop (length xs) ys)
tSuper x y                           = TError $ "Types `" ++ showPretty x ++ "` and `" ++ showPretty y ++ "` have no common supertype"

tMux :: Foldable f => f Type -> Type
tMux = foldr1 tSuper

tElement :: Type -> Type
tElement (TArray _ a [])             = a
tElement (TArray f a [_])            = maybe a eccType $ find eccAttr f
  where
    eccAttr (TAttr Ecc _ _) = True
    eccAttr _               = False
    eccType (TAttr Ecc _ t) = tReturn t
    eccType _               = undefined
tElement (TArray f a (_:bs))         = TArray f a bs
tElement (TConst a)                  = TConst $ tElement a
tElement (TInstance a _ _)           = tElement a
tElement TType{}                     = TError "Invalid element access; did you forget `typename`"
tElement x
    | not $ tResolved x              = TUnresolved
    | not $ tIsArray x               = TError $ "Invalid element access on non-array type `" ++ showPretty x ++ "`"
tElement _                           = TError "Invalid element access"

tElementLValue :: Type -> Type
tElementLValue (TArray _ a [])       = a
tElementLValue (TArray _ a [_])   = a
tElementLValue (TArray f a (_:bs))   = TArray f a bs
tElementLValue (TConst a)            = TConst $ tElement a
tElementLValue (TInstance a _ _)     = tElementLValue a
tElementLValue TType{}               = TError "Invalid element access; did you forget `typename`"
tElementLValue x
    | not $ tResolved x              = TUnresolved
    | not $ tIsArray x               = TError $ "Invalid element access on non-array type `" ++ showPretty x ++ "`"
tElementLValue _                     = TError "Invalid element access"

tField :: Type -> Name -> Type
tField t x = fromMaybe TUndefined $ tLookupField lookup t x

tLookupField :: (a -> [(Name, Type)] -> Maybe Type) -> Type -> a -> Maybe Type
tLookupField fn = member
  where
    member (TStruct _ a) x     = fn x a
    member (TUnion _ a) x      = fn x a
    member (TClass _ a) x      = fn x a
    member (TConst a) x        = TConst <$> member a x
    member (TReference _) _    = undefined
    member (TInstance a _ _) x = member a x
    member a _
        | not $ tResolved a    = Just TUnresolved
    member a             _     = Just $ TError $ "Invalid member access on type `" ++ showPretty a ++ "`"

noteType :: NotedExp n e -> NotedExp (Typed n) e
noteType = cata add
  where
    add (Note n e) = NotedExp (Typed TUnresolved n) e

unnoteType :: NotedExp (Typed n) e -> NotedExp n e
unnoteType = cata strip
  where
    strip (Note n e) = NotedExp (untype n) e

newNote :: Exp (Typed n) e -> NotedExp (Typed n) e -> NotedExp (Typed n) e
newNote e (NotedExp n _) = NotedExp (Typed TUnresolved $ untype n) e

localIdentifier :: QualifiedName -> NotedExp (Typed n) e -> NotedExp (Typed n) e
localIdentifier declScopeName ident = newNote (QualifiedIdentifierF scopedName) ident
  where
    scopedName = newNote (ScopedNameF declScopeName Nothing ident) ident

thisNamedValue :: QualifiedName -> NotedExp (Typed n) e -> NotedExp (Typed n) e
thisNamedValue declScopeName e = new $ NamedValueF $ localIdentifier declScopeName thisIdent
  where
    new = flip newNote e
    thisIdent = new $ IdentifierF thisName

qualifiedIdentifier :: QualifiedName -> QualifiedName -> NotedExp (Typed n) e -> NotedExp (Typed n) e
qualifiedIdentifier declScopeName name e = newNote (QualifiedIdentifierF $ fromJust $ scopedQualifier declScopeName e name) e

tResolved :: Type -> Bool
tResolved TAuto                = True
tResolved TTemplate            = False
tResolved TUndefined           = False
tResolved TUnresolved          = False
tResolved TUntyped             = True
tResolved (TInitializer xs)    = all tResolved xs
tResolved (TDesignator _ x)    = tResolved x
tResolved (TPositional _ x)    = tResolved x
tResolved TVoid                = True
tResolved TBoolean             = True
tResolved TFloat               = True
tResolved TString              = True
tResolved TSigned{}            = True
tResolved TUnsigned{}          = True
tResolved TError{}             = True
tResolved (TType x)            = tResolved x
tResolved (TDependent x)       = tResolved x
tResolved (TConst x)           = tResolved x
tResolved (TReference _)       = True
tResolved (TArray as x xs)     = tResolved x && not (null xs) && all attrResolved as
tResolved (TFunction _ _ x xs) = tResolved x && all (tResolved . typeOfParam) xs
tResolved (TClosure _ x y _)   = tResolved x && tResolved y
tResolved (TEnum _ x)          = tResolved x
tResolved (TStruct _ xs)       = all (tResolved . snd) xs
tResolved (TUnion _ xs)        = all (tResolved . snd) xs
tResolved (TClass _ xs)        = all (tResolved . snd) xs
tResolved (TInstance x _ _)    = tResolved x

attrResolved :: TAttr -> Bool
attrResolved (TAttr _ _ a) = tResolved a
attrResolved _             = True

isConstAuto :: NotedExp n e -> Bool
isConstAuto (Const Auto) = True
isConstAuto _ = False

resolveTypeOf :: Typed a -> Type -> Typed a
resolveTypeOf x t
    | tIsUndefined $ typeOf x = setTypeOf x t
     -- array w/ unknown dimensions is not tResolved, but may be a valid return type for [[pipelined]] functions
    | (TArray _ _ []) <- t    = setTypeOf x t
    | TDesignator{} <- t      = setTypeOf x t
    | TPositional{} <- t      = setTypeOf x t
    | TInitializer{} <- t     = setTypeOf x t
    | TTemplate <- t          = setTypeOf x t
    | tResolved t             = setTypeOf x t
    | tIsUndefined t          = setTypeOf x t
    | otherwise               = x

setTypeOf :: Typed a -> Type -> Typed a
setTypeOf x t = x { typeof = set $ typeof x }
  where
    set (TType TInstance{}) = undefined
    set (TInstance a b c)
        | tResolved a     = undefined
        | (TType t') <- t = TType (TInstance t' b c)
        | otherwise       = TInstance t b c
    set _ = t

setTypeOfExp :: Type -> NotedExp (Typed n) e -> NotedExp (Typed n) e
setTypeOfExp t (NotedExp n e) = NotedExp (setTypeOf n t) e

resetTypeOf :: Typed a -> Type -> Typed a
resetTypeOf x t = x { typeof = t }

resetTypeOfExp :: Type -> NotedExp (Typed n) e -> NotedExp (Typed n) e
resetTypeOfExp t (NotedExp n e) = NotedExp (resetTypeOf n t) e

intrinsics :: NotedExp (Typed n) e -> NotedExp (Typed n) e
intrinsics = cata resolve
  where
    resolve (Note n e) = NotedExp (maybe n (resolveTypeOf n) $ intrinsic e) e
      where
        intrinsic x@QualifiedIdentifierF{} = go x
        intrinsic x@ScopedNameF{} = go x
        intrinsic _ = Nothing

        go x = case getQualifiedName x :: [String] of
            ["__print"]         -> Just $ TFunction FreeFunction mempty TVoid [TFuncParam mempty TString Nothing]
            ["assert"]          -> Just $ TFunction FreeFunction mempty TVoid [TFuncParam mempty TBoolean Nothing]
            ["__cycles"]        -> Just $ TFunction FreeFunction mempty (TUnsigned 64) mempty
            ["__str_cnt"]       -> Just $ TFunction FreeFunction mempty (TUnsigned 64) mempty
            ["__assert_str_eq"] -> Just $ TFunction FreeFunction mempty TVoid [TFuncParam mempty TString Nothing, TFuncParam mempty TString Nothing]
            _                   -> Nothing


isIntLiteral :: NotedExp n e -> Bool
isIntLiteral (EnumValue _ x) = isIntLiteral x
isIntLiteral IntLiteral{}    = True
isIntLiteral _               = False

isUntypedIntLiteral :: NotedExp n e -> Bool
isUntypedIntLiteral (EnumValue _ x)        = isUntypedIntLiteral x
isUntypedIntLiteral (IntLiteral Nothing _) = True
isUntypedIntLiteral _                      = False

fromIntegralConstexpr :: Integral a => NotedExp n e -> Maybe a
fromIntegralConstexpr (IntLiteral _ x) = Just $ fromIntegral x
fromIntegralConstexpr (EnumValue _ x)  = fromIntegralConstexpr x
fromIntegralConstexpr _                = Nothing

fromNaturalConstexpr :: Integral a => NotedExp n e -> Maybe a
fromNaturalConstexpr = find (> 0) . fromIntegralConstexpr

inferExpType :: SymbolMap (Typed n) e -> Algebra (NotedExpF (Typed n) e) (NotedExp (Typed n) e)
inferExpType symbols (Note n e) = inferExpType' e
  where
    try = maybe TUnresolved

    isConstAutoVar (Variable _ _ a _ _ _) = isConstAuto a
    isConstAutoVar _ = False

    constIndexType = try (tConst . indexType) . fromIntegralConstexpr
      where
        indexType x
            | x < 0     = TError "The iteration count of `static for` can't be negative"
            | x < 3     = TUnsigned 1
            | otherwise = TUnsigned $ clog2 x

    setTypeOfTemplateCall t x@FunctionCallF{..}
        | t /= TAuto = x { callFunction = renote setType callFunction }
      where
        typ = TFunction FreeFunction mempty t mempty

        setType (FunctionSpecifierF a b) = FunctionSpecifierF a $ renote setType b
        setType (QualifiedIdentifierF a@TemplateInstance{})
            | not $ typeResolved a = QualifiedIdentifierF $ setTypeOfExp typ a
        setType a = a
    setTypeOfTemplateCall _ x = x

    inferExpType' RangeForF{..}
        | typeResolved forVar   = NotedExp n e
        | isConstAutoVar forVar = NotedExp n $ e { forVar = setTypeOfExp (inductionVarType forLimit) forVar }
      where
        inductionVarType x
            | isIntLiteral x = constIndexType x
            | otherwise      = try (tConst . TUnsigned) $ sizeOf x

    inferExpType' StaticForF{..}
        | typeResolved forVar   = NotedExp n e
        | isConstAutoVar forVar = NotedExp n $ e { forVar = setTypeOfExp (constIndexType forLimit) forVar }

    inferExpType' UnrolledForF{..}
        | typeResolved forVar   = NotedExp n e
        | isConstAutoVar forVar = NotedExp n $ e { forVar = setTypeOfExp (constIndexType forLimit) forVar }

    inferExpType' VariableF{..}
        | typeResolved n        = NotedExp n $ e { varInit = renote (setTypeOfTemplateCall $ typeOf n) <$> varInit }

    inferExpType' (AssignF a b)
        | typeResolved a        = NotedExp n $ AssignF a $ renote (setTypeOfTemplateCall $ typeOf a) b

    inferExpType' FunctionCallF{..}
        | typeResolved n && (tIsFunction (typeOf callFunction) || tIsClosure (typeOf callFunction))
                                = NotedExp n $ e { callArgs = renote (inferArgsFromParams $ map typeOfParam $ tFunctionParams $ typeOf callFunction) callArgs }
      where
        inferArgsFromParams ts (SeqF args)
            | length ts == length args = SeqF $ zipWith (renote . setTypeOfTemplateCall) ts args
        inferArgsFromParams _ x = x

    inferExpType' FunctionF{..}
        | typeResolved n        = NotedExp n $ e { body = renote (inferReturnStatement $ theType funcReturnType ) body }
      where
        inferReturnStatement t (SeqF stms) = SeqF $ map (renote returnStm) stms
          where
            returnStm (ReturnF (Just a)) = ReturnF $ Just $ renote (setTypeOfTemplateCall t) a
            returnStm a = a
        inferReturnStatement _ x = x

    inferExpType' _
        | typeResolved n        = NotedExp n e
        | otherwise             = NotedExp (resolveTypeOf n $ infer e) e

    infer AutoF                             = TUnresolved
    infer x@(AliasF _ t _)
        | tIsClosure $ typeOf n, (TInstance _ _ [(_, TTypeArg a), (_, TNameArg _ b), (_, TTypeArg c)]) <- typeOf n
                                            = TType $ TClosure b a c $ getQualifiedName x
        | otherwise                         = typeOf t
    infer (ArrayF a b c)                    = try TType $ TArray <$> attributes a <*> pure (theType b) <*> mapM fromIntegralConstexpr (toList $ unfix c)
    infer (ArrayAccessF a _)                = tElement $ typeOf a
    infer (ArrayAccessLValueF a _)          = tElementLValue $ typeOf a
    infer (BinaryF op a b)
        | (isIntLiteral a || (tIsConst (typeOf a) && tIsEnum (typeOf a))) &&
          (isIntLiteral b || (tIsConst (typeOf b) && tIsEnum (typeOf b)))
                                            = TUnresolved
        | not (typeResolved a) ||
          not (typeResolved b)              = TUnresolved
        | otherwise                         = inferBinaryOp op a b
    infer BitOffsetF{}                      = TUnresolved
    infer ByteOffsetF{}                     = TUnresolved
    infer BoolLiteralF{}                    = TBoolean
    infer BooleanF                          = TType TBoolean
    infer (CastF a _)                       = theType a
    infer x@CaptureF{}                      = typeOfSymbol x
    infer x@(ClassF _ _ a)
        | isLocalScope x                    = TUnresolved
        | otherwise                         = TType $ TClass (getQualifiedName x) (fields a)
    infer (DecltypeF _ a)                   = TType $ typeOf a
    infer (ConcatF a)                       = try TUnsigned (foldr (liftM2 (+) . sizeOf) (Just 0) $ unfix a)
    infer (ConstF a)                        = TType $ tConst $ theType a
    infer (DesignatorF a b)                 = TDesignator (getName a) (tUnConst $ typeOf b)
    infer x@(EnumF _ _ a _)                 = TType $ TEnum (getQualifiedName x) (theType a)
    infer x@EnumConstantF{}                 = try (tConst . theType) $ lookupSymbolByName (getQualifier x) symbols
    infer x@EnumValueF{}                    = try (tConst . theType) $ lookupSymbolByName (getQualifier x) symbols
    infer (FanOutF a b)                     = try (TArray Set.empty (typeOf b) . pure) (fromIntegralConstexpr a)
    infer FloatF                            = TType TFloat
    infer FloatLiteralF{}                   = TFloat
    infer FuncParamF{..}                    = theType paramType
    infer FunctionF{..}                     = funcType funcKind funcAttr (returnType funcReturnType) (params funcParams)
      where
        returnType Auto =
            maybe TVoid typeOf $ find returnExp $ unfix body
          where
            returnExp StaticIf{} = True
            returnExp Return{}   = True
            returnExp _          = False
        returnType a = theType a

    infer (FunctionCallF _ a b)             = elaborateType b $ tReturn $ typeOf a
    infer FunctionDeclF{..}                 = funcType FreeFunction funcAttr (theType funcReturnType) (params funcParams)
    infer (FunctionSpecifierF Nothing b)    = typeOf b
    infer (FunctionSpecifierF (Just a) b)   = method (typeOf a) (getName b)
    infer (FunctionTypeF a b Auto)          = TType $ funcType FreeFunction a TAuto (params b)
    infer (FunctionTypeF a b c)             = TType $ funcType FreeFunction a (theType c) (params b)
    infer (FunctionTypeParamF _ Auto _)     = TAuto
    infer (FunctionTypeParamF _ a _)        = theType a
    infer (InitializerListF a)              = TInitializer $ map typeOf $ toList $ unfix a
    infer (IntegerF x)                      = TType $ TSigned x
    infer (IntLiteralF w x)                 = inferIntLiteralType w x
    infer InterpolatedStringF{}             = TString
    infer LambdaF{}                         = TUnresolved
    infer (MemberAccessF a b)               = case typeOf a of
                                                  TReference s -> case lookupSymbolByName s symbols of
                                                                      Just (Class _ _ c) -> fromMaybe TUndefined $ lookup (getName b) (fields c :: [(String, Type)])
                                                                      _                  -> TUnresolved
                                                  a'           -> tField a' $ getName b
    infer (MuxF (IntLiteral _ x) a)         = try (tUnConst . typeOf) $ maybeAt (fromIntegral x) $ toList $ unfix a
    infer (MuxF (BoolLiteral x) a)          = try (tUnConst . typeOf) $ maybeAt (fromEnum x) $ toList $ unfix a
    infer (MuxF _ a)                        = tMux $ tUnConst . typeOf <$> unfix a
    infer (NamedValueF a)                   = typeOf a
    infer (QualifiedIdentifierF TemplateInstance{})
                                            = TUnresolved
    infer x@(QualifiedIdentifierF a)
        | typeOf a == TUnresolved           = TUnresolved
        | otherwise                         = typeOfSymbol x
    infer (ParamIntF x)                     = try (TType . TSigned) $ fromIntegralConstexpr x
    infer (ParamUintF x)                    = try (TType . TUnsigned) $ fromIntegralConstexpr x
    infer (PositionalF i a)                 = TPositional i $ tUnConst $ typeOf a
    infer (ReferenceF a)                    = TType $ TReference $ getQualifiedName a
    infer (ReturnF Nothing)                 = TVoid
    infer (ReturnF (Just a))                = tUnConst $ typeOf a
    infer (ScopedNameF _ Nothing a)
        | head (getName a) == '@'           = TUntyped
    infer (ScopedNameF _ _ a)
        | head (getName a) == '$'           = TUntyped
    infer x@(ScopedNameF _ b _)             = case lookupSymbol x symbols of
                                                  Just a@Alias{} -> nonErrorTypeOf a
                                                  Just _         -> TUntyped
                                                  Nothing
                                                     | maybe TUntyped typeOf b == TUnresolved -> TUnresolved
                                                     | otherwise                              -> TUndefined
    infer (StaticF a)                       = typeOf a
    infer StaticIfF{}                       = TUnresolved
    infer x@(StructF _ _ a)                 = TType $ TStruct (getQualifiedName x) (fields a)
    infer StringF                           = TType TString
    infer StringLiteralF{}                  = TString
    infer TemplateF{}                       = TTemplate
    infer TemplateInstanceF{}               = TUnresolved
    infer x@ThisF{}                         = case lookupSymbol x symbols of
                                                  Just x'@Class{} -> TReference $ getQualifiedName x'
                                                  Just x' -> typeOf x'
                                                  Nothing -> TUnresolved
    infer TypeParamF{}                      = TUnresolved
    infer (TypeIdentifierF _ TemplateInstance{})
                                            = TUnresolved
    infer x@(TypeIdentifierF _ a)
        | typeOf a == TUnresolved           = TUnresolved
        | otherwise                         = typeOfSymbol x
    infer x@(UnionF _ _ a)                  = TType $ TUnion (getQualifiedName x) (fields a)
    infer (UnaryF op a)
        | typeResolved a                    = inferUnaryOp op a
        | otherwise                         = TUnresolved
    infer (UnsignedF x)                     = TType $ TUnsigned x
    infer (WarningF _ _ x)                  = typeOf x
    infer (VariableF _ _ (Const Auto) _ a _) = try (tConst . typeOf) a
    infer (VariableF _ _ Auto _ a _)        = try (tUnConst . typeOf) a
    infer VariableF{..}                     = theType varType
    infer VoidF                             = TType TVoid

    infer AssignF{}                         = TUntyped
    infer AnnotatedF{}                      = TUntyped
    infer BarrierF                          = TUntyped
    infer DefaultInitializationF{}          = TUntyped
    infer ErrorF{}                          = TUntyped
    infer ExportTypeF{}                     = TUntyped
    infer ExternF{}                         = TUntyped
    infer FlagAttrF{}                       = TUntyped
    infer FunctionModifierF{}               = TUntyped
    infer IdentifierF{}                     = TUntyped
    infer IfF{}                             = TUntyped
    infer IntAttrF{}                        = TUntyped
    infer DoWhileF{}                        = TUntyped
    infer ModuleDeclF{}                     = TUntyped
    infer ModuleDiffF{}                     = TUntyped
    infer ModuleIdentifierF{}               = TUntyped
    infer NestedScopeF{}                    = TUntyped
    infer PrivateF                          = TUntyped
    infer PublicF                           = TUntyped
    infer RangeForF{}                       = TUntyped
    infer ReorderF{}                        = TUntyped
    infer SeqF{}                            = TUntyped
    infer StaticAssertF{}                   = TUntyped
    infer StaticAssertLegacyF{}             = TUntyped
    infer SwitchCaseF{}                     = TUntyped
    infer SwitchDefaultF{}                  = TUntyped
    infer SwitchF{}                         = TUntyped
    infer TemplateParamF{}                  = TUntyped
    infer AttrF{}                           = TUntyped
    infer TypenameF                         = TUntyped
    infer StaticForF{}                      = TUntyped
    infer UnrolledForF{}                    = TUntyped

    attributes = fmap Set.fromList . mapM attr . toList . unfix
      where
        attr (IntAttr x y)                  = TIntAttr x <$> fromIntegralConstexpr y
        attr (Attr x y)                     = Just $ TAttr x (getQualifiedName y) (typeOf y)
        attr (FlagAttr x)                   = Just $ TFlagAttr x
        attr _                              = undefined

    flags = Set.fromList . map attr . toList . unfix
      where
        attr (FlagAttr x)                   = TFlagAttr x
        attr (Warning _ _ x)                = attr x
        attr _                              = undefined

    typeOfSymbol x                          = maybe TUndefined nonErrorTypeOf $ lookupSymbol x symbols

    nonErrorTypeOf x = case typeOf x of
        TError{}   -> TUnresolved
        TUndefined -> TUnresolved
        t          -> t

    fields (Seq x) = mapMaybe member x
      where
        member m@Variable{}                 = Just (getName m, typeOf m)
        member StaticIf{}                   = Just ("", TUnresolved)
        member _                            = Nothing
    fields _                                = undefined

    params (Seq x)                          = mapMaybe param x
      where
        param p@(FunctionTypeParam a _ b)
            | typeOf p == TVoid             = Nothing
            | otherwise                     = Just $ TFuncParam (flags a) (typeOf p) (getName <$> b)
        param p                             = Just $ TFuncParam (flags $ paramAttr $ unNote $ unfix p) (typeOf p) (Just $ getName p)
    params _                                = undefined

    method :: Type -> Name -> Type
    method (TClass x _) y                   = try typeOf $ lookupSymbolByName (x ++ [y]) symbols
    method (TReference x) y                 = method (try theType $ lookupSymbolByName x symbols) y
    method (TInstance x _ _) y              = method x y
    method _ _                              = TUnresolved

    funcType k a r p                        = try funcType' $ attributes a
      where
        funcType' a'                        = TFunction k a' (quantifyReturnType r) p

        quantifyReturnType
            | any pipelined $ unfix a       = TDependent
            | otherwise                     = id
          where
            pipelined (FlagAttr Pipelined)  = True
            pipelined _                     = False

    elaborateType _ (TDependent TVoid)       = TVoid
    elaborateType (Seq []) (TDependent y)    = TArray Set.empty y []
    elaborateType (Seq (x:_)) (TDependent y) = TArray Set.empty y $ toList $ fromIntegralConstexpr x
    elaborateType _ y                        = y

{-# INLINE inferExpType #-}

data DeduceType = DeduceSupertype | DeduceSubtype
    deriving Eq

deduceTemplateArgs :: (Eq (Token s), Eq e) => SymbolMap (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
deduceTemplateArgs symbols = desugar' rewrite
  where
    functionArgTypes = map typeOf . toList . unfix

    rewrite n (MemberAccessF a b) | tIsUndefined (typeOf n) && tIsClass (typeOf a) =
        FunctionSpecifierF (Just a) (qualifiedIdentifier [] (tQualifiedName (typeOf a) ++ [getName b]) b)
    rewrite _ (FunctionSpecifierF (Just a) b) | getQualifier b == [unresolvedName] && tIsClass (typeOf a) =
        FunctionSpecifierF (Just a) (renote (setQualifier $ tQualifiedName $ typeOf a) b)
    rewrite _ (DesignatorF a b@(FunctionSpecifier x y)) =
        DesignatorF a $ copyNote (FunctionSpecifierF x $ deduceInstance [] y) b
    rewrite _ (FunctionCallF a b@(FunctionSpecifier x y) c) =
        FunctionCallF a (copyNote (FunctionSpecifierF x (deduceInstance (functionArgTypes c) y)) b) c
    rewrite _ (NamedValueF a@QualifiedIdentifier{}) | not $ typeResolved a = let a' = deduceInstance [] a in
        NamedValueF a'
    rewrite _ (TypeIdentifierF s t@TemplateInstance{}) =
        TypeIdentifierF s (deduceInstance [] t)
    rewrite _ (ScopedNameF s (Just t@TemplateInstance{}) a) =
        ScopedNameF s (Just (deduceInstance [] t)) a
    rewrite _ e = e

    resolveTemplateArgs t template@(Template params _) args deduced =
        fromMaybe args $ sequence $ Map.elems $ foldl substituteDefault (explicitArgs <> deducedArgs) params
      where
        new = flip newNote t
        explicitArgs = Map.fromList $ zip [0..] $ map Just args

        deducedArgs = Map.map arg deduced

        arg = \case
            Just (_, TTypeArg x)
                | tIsError x       -> Just $ new $ desugarError $ showPretty x
                | otherwise        -> new <$> expFromType t x
            Just (_, TIntArg x)    -> Just $ new $ IntLiteralF Nothing x
            Just (_, TNameArg _ x) -> Just $ qualifiedIdentifier [] x t
            _                      -> Nothing

        substituteDefault resolvedArgs (TemplateParam _ _ _ n x)
            | Just Nothing == deduced !? n = Map.insert n (desugar resolve <$> x) resolvedArgs
          where
            resolve e@(TypeParamF s _ _ index)
                | s == getQualifier template = maybe e (unNote . unfix) $ resolvedArgs ! index
            resolve e = removeNestedTypeIdentifier e
        substituteDefault x _ = x
    resolveTemplateArgs _ _ _ _ = undefined

    unresolvedArgs :: Foldable t => t a -> t a -> Map.Map Int (Maybe (DeduceType, TArg))
    unresolvedArgs params args = Map.fromList $ map (, Nothing) [length args..length params - 1]

    deduceInstance funcArgTypes templateInstance =
        deduceInstance' (lookupSymbol templateInstance symbols) templateInstance
      where
        deduceInstance' (Just template@(Template params (Function scope _ _ _ funcReturnType _ funcParams _))) e =
            deduceFromFunctionSignature template params scope funcReturnType funcParams e

        deduceInstance' (Just template@(Template params (FunctionDecl scope _ funcReturnType _ funcParams))) e =
            deduceFromFunctionSignature template params scope funcReturnType funcParams e

        deduceInstance' (Just template@(Template params _)) t@(TemplateInstance typ a@(Seq args))
            | length args < length params = copyNote (TemplateInstanceF typ $ copyNote resolvedArgs a) t
          where
            resolvedArgs = SeqF $ resolveTemplateArgs t template args $ unresolvedArgs params args

        deduceInstance' _ e = e

        deduceFromFunctionSignature template params scope funcReturnType funcParams q@(QualifiedIdentifier ident)
            | length args <= length params = copyNote (QualifiedIdentifierF deducedInstance) q
            | otherwise = q
          where
            decomposeInstance (TemplateInstance x y@(Seq z)) = (x, y, z)
            decomposeInstance _ = (ident, copyNote (SeqF []) ident, [])

            (func, argsSeq, args) = decomposeInstance ident

            deducedInstance = copyNote (TemplateInstanceF func resolvedArgs) q

            resolvedArgs = copyNote (SeqF $ zipWith deduceInstanceFromTemplateParam params resolvedArgs') argsSeq
              where
                resolvedArgs' = resolveTemplateArgs q template args
                              $ execState (runReaderT deduction DeduceSupertype) (unresolvedArgs params args)

                deduceInstanceFromTemplateParam (TemplateParam _ (TypeParam _ Typename _ i) _ _ _) e =
                    renote (deduceInstanceFromType $ resolvedArgs' !! i) e
                deduceInstanceFromTemplateParam (TemplateParam _ a _ _ _) e =
                    renote (deduceInstanceFromType a) e
                deduceInstanceFromTemplateParam _ e = e

                deduceInstanceFromType (FunctionType _ (Seq a) _) (NamedValueF b)
                    | not $ typeResolved b = NamedValueF $ deduceInstance (map typeOf a) b
                deduceInstanceFromType _ e = e

            deduction = do
                zipWithM_ deduceFromFuncParam (toList $ unfix funcParams) funcArgTypes
                zipWithM_ deduceFromTemplateParam params (map typeOf args)
                when (tIsFunction $ typeOf ident) $
                    deduceType funcReturnType (tReturn $ typeOf ident)

            setArg p@(TypeParam scope' _ _ index) arg =
                when (scope' == scope) $ do
                    how <- ask
                    modify $ Map.adjust (reconcile $ Just (how, arg)) index
              where
                reconcile (Just (_, TTypeArg TInitializer{})) y = y
                reconcile x Nothing = x
                reconcile x@(Just (DeduceSupertype, TTypeArg tx)) y@(Just (DeduceSupertype, TTypeArg ty))
                    | (tIsInt tx || tIsEnum tx) &&
                      (tIsInt ty || tIsEnum ty) = Just (DeduceSupertype, TTypeArg $ tSuper tx ty)
                    | tx <: ty = y
                    | ty <: tx = x
                reconcile (Just (DeduceSubtype, TTypeArg tx)) y@(Just (_, TTypeArg ty))
                    | ty <: tx = y
                reconcile x@(Just (_, TTypeArg tx)) (Just (DeduceSubtype, TTypeArg ty))
                    | tx <: ty = x
                reconcile x@(Just (_, TTypeArg tx)) y@(Just (_, TTypeArg ty))
                    | not $ tResolved tx = x
                    | not $ tResolved ty = y
                    | TError{} <- ty = y
                reconcile x y
                    | (snd <$> x) == (snd <$> y) = x
                    | otherwise = Just (DeduceSupertype, TTypeArg $ TError (
                                       "Conflicting template argument deduction results for parameter `" ++ showPretty p ++ "`. Could be `"
                                       ++ showPretty (snd <$> x) ++ "` or `" ++ showPretty (snd <$> y) ++ "`."))

            setArg _ _ = undefined

            deduceFromFuncParam (FuncParam _ _ t _) = deduceType t
            deduceFromFuncParam _ = undefined

            deduceFromTemplateParam (TemplateParam _ t@FunctionType{} _ _ _) = deduceType t
            deduceFromTemplateParam _ = const $ return ()

            deduceArg x = \case
                TTypeArg y   -> deduceType x y
                TIntArg y    -> void $ deduceInt x y
                TNameArg t y -> deduceName x (t, y)
                _            -> return ()

            deduceType _ TAuto = return ()

            deduceType (FunctionType _ a r) (TFunction _ _ r' a') = do
                deduceType r r'
                local (const DeduceSubtype) $
                    zipWithM_ deduceType (map paramType $ toList $ unfix a) (map typeOfParam a' ++ repeat TVoid)
              where
                paramType (FunctionTypeParam _ e _) = e
                paramType _ = undefined

            deduceType (TypeIdentifier _ (TemplateInstance a b)) (TInstance _ a' b')
                | getQualifiedName a == a' = zipWithM_ deduceArg (toList $ unfix b) (map snd b')

            deduceType (TypeIdentifier _ t@(TemplateInstance _ b)) (TInitializer x) =
                deduce $ lookupSymbol t symbols
              where
                deduce (Just (Template _ (Struct _ _ a))) = zipWithM_ deduceFromFieldType (toList $ unfix a) (map tPositionalType x)
                deduce _ = return ()
                deduceFromFieldType (Variable _ _ (TypeParam _ _ _ index) _ _ _) = deduceType (toList (unfix b) !! index)
                deduceFromFieldType _ = const $ return ()

            deduceType (TypeIdentifier _ (TemplateInstance _ b)) t
                | not $ tResolved t = zipWithM_ deduceArg (toList $ unfix b) $ repeat $ TTypeArg TUnresolved

            deduceType a (TInstance t _ _) = deduceType a t

            deduceType (Const a) t = deduceType a t

            deduceType a (TConst t) = deduceType a t

            deduceType a@(Array _ _ (Seq _)) t@(TArray _ _ []) = do
                deduceType (element a) (tElement t)

            deduceType a@(Array _ _ (Seq (a':_))) t@(TArray _ _ (t':_)) = do
                x <- deduceInt a' t'
                when x $ deduceType (element a) (tElement t)

            deduceType a@(Array _ _ (Seq (a':_))) (TInitializer ts) = do
                unless (null ts) $ void $ deduceInt a' $ fromIntegral $ length ts
                forM_ ts $ deduceType (element a) . tPositionalType

            deduceType (ParamInt p@TypeParam{}) (TSigned x) =
                setArg p $ TIntArg $ fromIntegral x

            deduceType (ParamInt p@TypeParam{}) (TUnsigned x) =
                setArg p $ TIntArg $ fromIntegral (x + 1)

            deduceType (ParamUint p@TypeParam{}) (TUnsigned x) =
                setArg p $ TIntArg $ fromIntegral x

            deduceType p@(TypeParam _ Typename _ _) t =
                setArg p (TTypeArg t)

            deduceType _ _ = return ()

            deduceName p@(TypeParam _ a _ _) (t, s) = do
                setArg p $ TNameArg t s
                deduceType a t

            deduceName (NamedValue a) x = deduceName a x
            deduceName _ _              = return ()

            deduceInt (IntLiteral _ x) y = return (x == y)
            deduceInt p@TypeParam{} y    = setArg p (TIntArg y) $> True
            deduceInt _ _                = return False

        deduceFromFunctionSignature _ _ _ _ _ e = e

    element x@(Array a b c@(Seq (_:xs)))
        | null xs   = b
        | otherwise = newNote (ArrayF a b $ copyNote (SeqF xs) c) x
    element _ = undefined

deduceAuto :: NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
deduceAuto = desugar' auto
  where
    typeExp e err t = NotedExp (Typed (TType t) (untype $ note $ unfix e)) (fromMaybe err $ expFromType e t)

    auto n e@FunctionF{..}
        | isAuto funcReturnType = e { funcReturnType = typeExp funcReturnType err $ tReturn $ typeof n }
      where
        err = desugarError "Could not deduce function return type"

    auto n e@VariableF{..}
        | isAuto varType || isConstAuto varType = e { varType = typeExp varType err $ typeof n }
      where
        err = desugarError "Could not deduce variable type"

    auto _ e = e

expFromType :: NotedExp (Typed n) (ParseError s e) -> Type -> Maybe (Exp (Typed n) (ParseError s e))
expFromType e = typ
  where
    typ TAuto                 = Just AutoF
    typ TTemplate             = Nothing
    typ (TType x)             = typ x
    typ TInitializer{}        = Nothing
    typ TDesignator{}         = Nothing
    typ TPositional{}         = Nothing
    typ TVoid                 = Just VoidF
    typ TBoolean              = Just BooleanF
    typ TFloat                = Just FloatF
    typ TString               = Just StringF
    typ (TSigned n)           = Just $ IntegerF n
    typ (TUnsigned n)         = Just $ UnsignedF n
    typ (TDependent x)        = typ x
    typ (TConst x)            = ConstF <$> typ' x
    typ (TReference s)        = Just $ ReferenceF $ typeIdentifier [] s e
    typ (TArray _ _ [])       = Nothing
    typ (TArray as x xs)      = ArrayF <$> attributes as <*> typ' x <*> dims xs
    typ (TFunction _ as x xs) = FunctionTypeF <$> attributes as <*> params xs <*> typ' x
    typ (TClosure _ _ _ s)    = typeIdentifier' s
    typ (TEnum s _)           = typeIdentifier' s
    typ (TStruct s _)         = typeIdentifier' s
    typ (TUnion s _)          = typeIdentifier' s
    typ (TClass s _)          = typeIdentifier' s
    typ (TInstance t _ _)     = typ t
    typ TError{}              = Nothing
    typ TUndefined            = Nothing
    typ TUnresolved           = Nothing
    typ TUntyped              = Nothing

    noteWithType t = resetTypeOfExp t . flip newNote e
    noteUntyped = noteWithType TUntyped

    typ' x = noteWithType (TType x) <$> typ x

    typeIdentifier' s = Just $ unNote $ unfix $ typeIdentifier [] s e

    mapSeq g f xs = noteUntyped . SeqF <$> mapM go xs
      where
        go x = noteWithType (g x) <$> f x

    attributes = mapSeq (const TUntyped) Just . expFromTAttr e
    dims = mapSeq (inferIntLiteralType Nothing) (Just . IntLiteralF Nothing)
    params = mapSeq typeOfParam param
      where
        param p = FunctionTypeParamF <$> attributes (attrOfParam p) <*> typ' (typeOfParam p) <*> pure (ident <$> nameOfParam p)
        ident = noteUntyped . IdentifierF

expFromTAttr ::  NotedExp (Typed n) (ParseError s e) -> Set TAttr -> [Exp (Typed n) (ParseError s e)]
expFromTAttr e = map attr . Set.toList
  where
    attr (TAttr a b _)  = AttrF a $ qualifiedIdentifier [] b e
    attr (TIntAttr a b) = IntAttrF a $ noteWithType (inferIntLiteralType Nothing b) (IntLiteralF Nothing b)
    attr (TFlagAttr a)  = FlagAttrF a

    noteWithType t = resetTypeOfExp t . flip newNote e
