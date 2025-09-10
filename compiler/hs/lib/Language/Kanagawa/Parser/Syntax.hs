{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Syntax
    ( ExpF(..)
    , Exp
    , DocComment(..)
    , Note(..)
    , Src(..)
    , NotedExp
    , NotedExpF
    , pattern NotedExp
    , BinaryOp(..)
    , UnaryOp(..)
    , Attribute(..)
    , Flag(..)
    , Modifier(..)
    , FunctionKind(..)
    , DeclareFlag(..)
    , Format(..)
    , FormatSpecifier(..)
    , Name
    , toString
    , mapName
    , QualifiedName
    , LiteralType(..)
    , LiteralTypeSignedness(..)
    , NameLike(..)
    , ExpLike(..)
    , thisName
    , unresolvedName
    , moduleNamespace
    , unmangleModuleNamespace
    , WarningKind(..)
    ) where

import Data.Hashable
import Data.String
import Prettyprinter
import Language.Kanagawa.Recursion
import Language.Kanagawa.Warning
import Text.Megaparsec

newtype Name = Name (Hashed String)
    deriving (Eq, Ord, Hashable, IsString)

unName :: Name -> Hashed String
unName (Name h) = h

toString :: Name -> String
toString = unhashed . unName

mapName :: (String -> String) -> Name -> Name
mapName f = Name . mapHashed f . unName

instance Show Name where
    show = show . toString

instance Pretty Name where
    pretty = pretty . toString

type QualifiedName = [Name]

-- Enum types' constructors must be defined in the same order
-- as values in coresponding C enums defined in parse_tree.h

-- ParseTreeBinaryOpType
data BinaryOp =
    Add | Sub | Mul | LutMul | Div | Mod |
    BitwiseAnd | And |
    BitwiseOr | Or |
    BitwiseXor | Xor |
    ShiftLeft | ShiftRight |
    Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual
    deriving (Eq, Ord, Show, Enum)

instance Pretty BinaryOp where
    pretty Add          = "+"
    pretty Sub          = "-"
    pretty Mul          = "*"
    pretty LutMul       = undefined
    pretty Div          = "/"
    pretty Mod          = "%"
    pretty BitwiseAnd   = "&"
    pretty And          = "&&"
    pretty BitwiseOr    = "|"
    pretty Or           = "||"
    pretty BitwiseXor   = "^"
    pretty Xor          = "^^"
    pretty ShiftLeft    = "<<"
    pretty ShiftRight   = ">>"
    pretty Equal        = "=="
    pretty NotEqual     = "!="
    pretty Greater      = ">"
    pretty GreaterEqual = ">="
    pretty Less         = "<"
    pretty LessEqual    = "<="

-- ParseTreeUnaryOpType
data UnaryOp =
    Invert |
    Neg |
    Not |
    BitSizeof |
    ByteSizeof |
    Clog2
    deriving (Eq, Ord, Show, Enum)

instance Pretty UnaryOp where
    pretty Invert     = "~"
    pretty Neg        = "-"
    pretty Not        = "!"
    pretty BitSizeof  = "bitsizeof"
    pretty ByteSizeof = "bytesizeof"
    pretty Clog2      = "clog2"

data Attribute =
    CallRate |
    Ecc |
    FifoDepth |
    Latency |
    MaxThreads |
    Rename |
    Schedule |
    ThreadRate |
    TransactionSize
    deriving (Eq, Ord)

instance Show Attribute where
    show CallRate        = "call_rate"
    show Ecc             = "ecc"
    show FifoDepth       = "fifo_depth"
    show Latency         = "latency"
    show MaxThreads      = "max_threads"
    show Rename          = "name"
    show Schedule        = "schedule"
    show ThreadRate      = "thread_rate"
    show TransactionSize = "transaction_size"

instance Pretty Attribute where
    pretty = viaShow

data Flag =
    Async |
    Atomic |
    EndTransaction |
    Initialize |
    Memory |
    NoBackPressure |
    Pure |
    NonReplicated |
    Pipelined |
    QuadPort |
    ReorderByLooping |
    Reset |
    Unordered
    deriving (Eq, Ord)

instance Show Flag where
    show Async            = "async"
    show Atomic           = "atomic"
    show EndTransaction   = "last"
    show Memory           = "memory"
    show Initialize       = "initialize"
    show NoBackPressure   = "no_backpressure"
    show Pure             = "pure"
    show NonReplicated    = "non_replicated"
    show Pipelined        = "pipelined"
    show QuadPort         = "quad_port"
    show ReorderByLooping = "reorder_by_looping"
    show Reset            = "reset"
    show Unordered        = "unordered"

instance Pretty Flag where
    pretty = viaShow

data DeclareFlag =
    DeclareGlobal |
    DeclareClass |
    DeclareLocal |
    DeclareStatic |
    DeclareMember |
    DeclareUninitConst
    deriving (Eq, Ord, Show)

data Modifier =
     Inline |
     NoInline
     deriving (Eq, Ord)

instance Show Modifier where
    show Inline         = "inline"
    show NoInline       = "noinline"

instance Pretty Modifier where
    pretty = viaShow

data FunctionKind =
    FreeFunction |
    MemberFunction |
    LambdaFunction
    deriving (Eq, Ord, Show)

data LiteralTypeSignedness =
    SignedLiteral |
    UnsignedLiteral
    deriving (Eq, Ord)

data LiteralType =
    LiteralType
    { literalSignedness :: LiteralTypeSignedness
    , literalWidth :: Int
    }
    deriving (Eq, Ord, Show)

instance Show LiteralTypeSignedness where
    show SignedLiteral = "i"
    show UnsignedLiteral = "u"

instance Pretty LiteralTypeSignedness where
    pretty = viaShow

instance Pretty LiteralType where
    pretty a = pretty (literalSignedness a) <> pretty (literalWidth a)

data FormatSpecifier =
    Bin |
    Oct |
    Dec |
    Hex |
    HexUpper
    deriving (Eq, Ord)

instance Show FormatSpecifier where
    show Bin      = "b"
    show Oct      = "o"
    show Dec      = "d"
    show Hex      = "x"
    show HexUpper = "X"

instance Pretty FormatSpecifier where
    pretty = viaShow

data Format =
    Format
    { formatSpecifier :: FormatSpecifier
    , formatPrecision :: Maybe Int
    }
    deriving (Eq, Ord)

instance Show Format where
    show Format{..} = ":" <> show formatSpecifier <> maybeShow formatPrecision
      where
        maybeShow Nothing = mempty
        maybeShow (Just x) = show x

instance Pretty Format where
    pretty = viaShow

thisName :: Name
thisName = "$this"

unresolvedName :: Name
unresolvedName = "$unresolved"

moduleNamespace :: [String] -> Name
moduleNamespace = fromString . concatMap ('@':)

unmangleModuleNamespace :: Name -> Name
unmangleModuleNamespace = mapName unmangle
  where
    unmangle ('@':xs) = map (\c -> if c == '@' then '.' else c) xs
    unmangle xs = xs

data ExpF e a =
    -- List
    SeqF [a]
    |
    -- Attributes
    FlagAttrF Flag
    |
    IntAttrF Attribute a
    |
    AttrF Attribute a
    |
    FunctionModifierF Modifier
    |
    -- Type
    AutoF
    |
    VoidF
    |
    BooleanF
    |
    FloatF
    |
    StringF
    |
    IntegerF Int
    |
    ParamIntF a
    |
    UnsignedF Int
    |
    ParamUintF a
    |
    ConstF a
    |
    ReferenceF a
    |
    ArrayF a a a
    |
    TemplateInstanceF a a
    |
    TemplateParamF QualifiedName a Name Int (Maybe a)
    |
    TypeParamF QualifiedName a Name Int
    |
    TypenameF
    |
    FunctionTypeParamF a a (Maybe a)
    |
    FunctionTypeF a a a
    |
    -- Expression
    ThisF
        { declScopeName :: QualifiedName
        }
    |
    DesignatorF a a
    |
    PositionalF Int a
    |
    InitializerListF a
    |
    BitOffsetF a a
    |
    ByteOffsetF a a
    |
    IntLiteralF
        { literalType :: Maybe LiteralType
        , literalValue :: Integer
        }
    |
    FloatLiteralF Float
    |
    BoolLiteralF Bool
    |
    StringLiteralF String
    |
    InterpolatedStringF [(String, Maybe (a, Bool, Maybe a, Maybe Format))]
    |
    IdentifierF Name
    |
    ScopedNameF QualifiedName (Maybe a) a
    |
    QualifiedIdentifierF a
    |
    TypeIdentifierF QualifiedName a
    |
    FunctionSpecifierF (Maybe a) a
    |
    EnumValueF a a
    |
    NamedValueF a
    |
    BinaryF BinaryOp a a
    |
    UnaryF UnaryOp a
    |
    FunctionCallF
        { callAttr :: a
        , callFunction :: a
        , callArgs :: a
        }
    |
    ArrayAccessF a a
    |
    ArrayAccessLValueF a a
    |
    MemberAccessF a a
    |
    CastF a a
    |
    MuxF a a
    |
    ConcatF a
    |
    FanOutF a a
    |
    StaticF a
    |
    -- Statement
    AnnotatedF a a
    |
    BarrierF
    |
    ReorderF a
    |
    ReturnF (Maybe a)
    |
    DoWhileF
        { loopAttr :: a
        , loopCond :: a
        , body :: a
        }
    |
    UnrolledForF
        { forVar :: a
        , forLimit :: a
        , body :: a
        }
    |
    StaticForF
        { forVar :: a
        , forLimit :: a
        , body :: a
        }
    |
    RangeForF
        { loopAttr :: a
        , forVar :: a
        , forLimit :: a
        , body :: a
        }
    |
    IfF
        { ifCond :: a
        , ifThen :: a
        , ifElse :: Maybe a
        }
    |
    StaticIfF
        { staticIfCond :: a
        , staticIfThen :: a
        , staticIfElse :: Maybe a
        }
    |
    SwitchF a a
    |
    SwitchCaseF a a
    |
    SwitchDefaultF a
    |
    AssignF a a
    -- Declaration
    |
    NestedScopeF
        { declScopeName :: QualifiedName
        , declName :: a
        , body :: a
        }
    |
    ModuleIdentifierF [String]
    |
    ModuleDiffF [String] [String]
    |
    ModuleDeclF
        { declName :: a
        , exposed :: [a]
        , body :: a
        }
    |
    EnumConstantF
        { declScopeName :: QualifiedName
        , declName :: a
        , constValue :: Maybe a
        }
    |
    EnumF
        { declScopeName :: QualifiedName
        , declName :: a
        , enumType :: a
        , body :: a
        }
    |
    StructF
        { declScopeName :: QualifiedName
        , declName :: a
        , body :: a
        }
    |
    UnionF
        { declScopeName :: QualifiedName
        , declName :: a
        , body :: a
        }
    |
    ClassF
        { declScopeName :: QualifiedName
        , declName :: a
        , body :: a
        }
    |
    DefaultInitializationF a
    |
    PrivateF
    |
    PublicF
    |
    TemplateF
        { templateParams :: [a]
        , templateDecl :: a
        }
    |
    DecltypeF
        { declScopeName :: QualifiedName
        , declExp :: a
        }
    |
    AliasF
        { declScopeName :: QualifiedName
        , body :: a
        , declName :: a
        }
    |
    CaptureF
        { declScopeName :: QualifiedName
        , declName :: a
        }
    |
    VariableF
        { declScopeName :: QualifiedName
        , varAttr :: a
        , varType :: a
        , declName :: a
        , varInit :: Maybe a
        , declFlags :: DeclareFlag
        }
    |
    FuncParamF
        { declScopeName :: QualifiedName
        , paramAttr :: a
        , paramType :: a
        , declName :: a
        }
    |
    FunctionF
        { declScopeName :: QualifiedName
        , funcKind :: FunctionKind
        , funcAttr :: a
        , funcModifier :: Maybe a
        , funcReturnType :: a
        , declName :: a
        , funcParams :: a
        , body :: a
        }
    |
    FunctionDeclF
        { declScopeName :: QualifiedName
        , funcAttr :: a
        , funcReturnType :: a
        , declName :: a
        , funcParams :: a
        }
    |
    LambdaF
        { declScopeName :: QualifiedName
        , declName :: a
        , lambdaCaptures :: a
        , funcParams :: a
        , funcReturnType :: a
        , body :: a
        }
    |
    ExportTypeF a a
    |
    ExternF a a
    |
    StaticAssertF a
    |
    StaticAssertLegacyF a
    |
    WarningF WarningKind e a
    |
    ErrorF e
    deriving (Eq, Ord, Foldable, Functor, Traversable, Show)

data Note x f a =
    Note
    { note   :: x       -- ^ the note
    , unNote :: !(f a)  -- ^ the original functor
    }
    deriving (Show,Functor,Foldable,Traversable)

-- Equality ignores annotation
instance Eq (f a) => Eq (Note n f a) where
    (==) a b = unNote a == unNote b

-- Ordering ignores annotation
instance Ord (f a) => Ord (Note n f a) where
    (<=) a b = unNote a <= unNote b

data DocComment = LineComment  [String]
                | BlockComment [String]
  deriving (Eq, Read, Show)

instance Pretty DocComment where
    pretty (LineComment  xs) = pretty $ unlines xs
    pretty (BlockComment xs) = pretty $ unlines xs

-- | Source code location
data Src =
    Src
        { offset :: !Int
        , begin :: !SourcePos
        , end :: !SourcePos
        , docs :: ([DocComment], [DocComment])
        }
    |
    SrcStack Src Src
    |
    SrcUnknown
  deriving (Eq, Show)

instance Semigroup Src where
    a <> (SrcStack b c) = SrcStack a (SrcStack b c)
    (SrcStack a b) <> c = SrcStack a (SrcStack c b)
    a <> b              = SrcStack a b

type Exp n e = ExpF e (NotedExp n e)

type NotedExpF n e = Note n (ExpF e)

type NotedExp n e = Fix (NotedExpF n e)

pattern NotedExp :: n -> f (Fix (Note n f)) -> Fix (Note n f)
pattern NotedExp n e = Fix (Note n e)

{-# COMPLETE NotedExp #-}

class NameLike s where
    fromName :: Name -> s

instance NameLike Name where
    fromName = id

instance NameLike String where
    fromName = toString

instance NameLike (Doc ann) where
    fromName = pretty

class ExpLike a where
    isType :: a -> Bool

    isTemplate :: a -> Bool

    isAuto :: a -> Bool

    isConst :: a -> Bool

    isDiagnostic :: a -> Bool

    isFunction :: a -> Bool

    isAbbreviatedTemplate :: a -> Bool

    getName :: (IsString s, NameLike s) => a -> s

    getQualifier :: a -> QualifiedName

    getScope :: a -> QualifiedName

    getQualifiedName :: (IsString s, NameLike s) => a -> [s]
    getQualifiedName e = map fromName $ getQualifier e ++ [getName e]

instance ExpLike (Exp n e) where
    isType AutoF                            = True
    isType VoidF                            = True
    isType BooleanF                         = True
    isType FloatF                           = True
    isType StringF                          = True
    isType ConstF{}                         = True
    isType ReferenceF{}                     = True
    isType IntegerF{}                       = True
    isType UnsignedF{}                      = True
    isType ParamIntF{}                      = True
    isType ParamUintF{}                     = True
    isType EnumF{}                          = True
    isType StructF{}                        = True
    isType ClassF{}                         = True
    isType UnionF{}                         = True
    isType AliasF{}                         = True
    isType TypeIdentifierF{}                = True
    isType ArrayF{}                         = True
    isType FunctionTypeF{}                  = True
    isType (TemplateF _ a)                  = isType a
    isType (TypeParamF _ a _ _)             = not $ isType a
    isType (TemplateParamF _ a _ _ _)       = not $ isType a
    isType (TemplateInstanceF a _)          = isType a
    isType _                                = False

    isTemplate TemplateF{}                  = True
    isTemplate (TemplateParamF _ a _ _ _)   = isTemplate a
    isTemplate (TypeParamF _ a _ _)         = isTemplate a
    isTemplate _                            = False

    isAuto AutoF                            = True
    isAuto _                                = False

    isConst ConstF{}                        = True
    isConst _                               = False

    isDiagnostic ErrorF{}                   = True
    isDiagnostic WarningF{}                 = True
    isDiagnostic _                          = False

    isFunction FunctionF{}                  = True
    isFunction LambdaF{}                    = True
    isFunction FunctionDeclF{}              = True
    isFunction (TemplateF  _ a)             = isFunction a
    isFunction _                            = False

    isAbbreviatedTemplate FunctionF{..}     = any (isAuto . paramType . unNote . unfix) $ unfix funcParams
    isAbbreviatedTemplate FunctionDeclF{..} = any (isAuto . paramType . unNote . unfix) $ unfix funcParams
    isAbbreviatedTemplate _                 = False

    getName (IdentifierF s)                 = fromName s
    getName (DesignatorF a _)               = getName a
    getName (TypeIdentifierF _ a)           = getName a
    getName (QualifiedIdentifierF a)        = getName a
    getName (EnumValueF a _)                = getName a
    getName (NamedValueF a)                 = getName a
    getName (TemplateF _ a)                 = getName a
    getName (TemplateInstanceF a _)         = getName a
    getName (ScopedNameF _ _ a)             = getName a
    getName (DecltypeF _ a)                 = getName a
    getName (FunctionSpecifierF _ a)        = getName a
    getName (TypeParamF _ _ s _)            = fromName s
    getName (TemplateParamF _ _ s _ _)      = fromName s
    getName ThisF{}                         = fromName thisName
    getName (WarningF _ _ a)                = getName a
    getName ErrorF{}                        = ""
    getName SeqF{}                          = ""
    getName (ModuleIdentifierF n)           = fromName $ moduleNamespace n
    getName e                               = getName $ declName e

    getQualifier IdentifierF{}              = []
    -- parser always makes TypeIdentifier fully qualified
    getQualifier (TypeIdentifierF _ a)      = getQualifier a
    getQualifier (QualifiedIdentifierF a)   = getQualifier a
    getQualifier (EnumValueF a _)           = getQualifier a
    getQualifier (NamedValueF a)            = getQualifier a
    getQualifier (FunctionSpecifierF _ a)   = getQualifier a
    getQualifier (TypeParamF a _ _ _)       = a
    getQualifier (TemplateParamF a _ _ _ _) = a
    getQualifier (TemplateF _ a)            = getQualifier a
    getQualifier (TemplateInstanceF a _)    = getQualifier a
    getQualifier (ScopedNameF _ Nothing _)  = []
    getQualifier (ScopedNameF _ (Just a) _) = getQualifier a ++ [getName a]
    getQualifier ThisF{}                    = []
    getQualifier (WarningF _ _ a)           = getQualifier a
    getQualifier ErrorF{}                   = []
    getQualifier SeqF{}                     = []
    getQualifier ModuleIdentifierF{}        = []
    getQualifier e                          = declScopeName e

    getScope IdentifierF{}                  = []
    getScope (ScopedNameF n _ _)            = n
    getScope (TypeIdentifierF _ a)          = getScope a
    getScope (QualifiedIdentifierF a)       = getScope a
    getScope (TemplateInstanceF a _)        = getScope a
    getScope (FunctionSpecifierF _ a)       = getScope a
    getScope (TypeParamF n _ _ _)           = n
    getScope (TemplateF _ a)                = getScope a
    getScope (EnumValueF a _)               = getScope a
    getScope (NamedValueF a)                = getScope a
    getScope (ThisF n)                      = n
    getScope (WarningF _ _ a)               = getScope a
    getScope ErrorF{}                       = []
    getScope SeqF{}                         = []
    getScope e                              = declScopeName e

instance ExpLike (ExpF e a) => ExpLike (NotedExpF n e a) where
    isType                                  = isType . unNote
    isTemplate                              = isTemplate . unNote
    isAuto                                  = isAuto . unNote
    isConst                                 = isConst . unNote
    isDiagnostic                            = isDiagnostic . unNote
    isFunction                              = isFunction . unNote
    isAbbreviatedTemplate                   = isAbbreviatedTemplate . unNote
    getName                                 = getName . unNote
    getQualifier                            = getQualifier . unNote
    getScope                                = getScope . unNote

instance ExpLike (NotedExp n e) where
    isType                                  = isType . unNote . unfix
    isTemplate                              = isTemplate . unNote . unfix
    isAuto                                  = isAuto . unNote . unfix
    isConst                                 = isConst . unNote . unfix
    isDiagnostic                            = isDiagnostic . unNote . unfix
    isFunction                              = isFunction . unNote . unfix
    isAbbreviatedTemplate                   = isAbbreviatedTemplate . unNote . unfix
    getName                                 = getName . unNote . unfix
    getQualifier                            = getQualifier . unNote . unfix
    getScope                                = getScope . unNote . unfix

