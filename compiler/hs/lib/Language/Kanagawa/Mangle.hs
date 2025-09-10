{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Mangle
    ( mangleTemplateScopedName
    , mangleTemplateIdentifier
    ) where

import Language.Kanagawa.Desugar
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.Pattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Type

mangleTemplateScopedName :: Exp n e -> NotedExp n e -> Exp n e -> Exp n e
mangleTemplateScopedName template args (ScopedNameF n a b) = ScopedNameF n a $ mangleTemplateIdentifier template args b
mangleTemplateScopedName _ _ _ = undefined

-- Mangling algorithm must generate unique names for different sets of arguments.
-- 1) Mangled arguments are wrapped between <| and |>
-- 2) Strings contributed by the compiler are prefixed with $ (dollar sign).
-- 3) User defined names and scalar literals are prefixed with @ (ampersand).
mangleTemplateIdentifier :: Exp n e -> NotedExp n e -> NotedExp n e -> NotedExp n e
mangleTemplateIdentifier template = renote . mangleIdentifier'
  where
    mangleIdentifier' args (IdentifierF a) = IdentifierF $ suffixName ("<|" <> cata mangle args <> "|>") a
    mangleIdentifier' _ _ = undefined

    isClosure = getQualifiedName template == data'closure'core'Closure

    mangle Auto                        = "$auto"
    mangle (Array x y z)               = "$array" <> x <> y <> z
    mangle (Attr x y)                  = "@" <> show x <> y
    mangle (BoolLiteral x)             = "@" <> show x
    mangle (Const x)                   = "$const" <> x
    mangle (Designator x y)            = "@" <> x <> y
    mangle (EnumValue x _)             = x
    mangle (FlagAttr x)                = "$" <> show x
    mangle (FloatLiteral x)            = "@" <> show x
    mangle (FunctionType x y z)
        | isClosure                    = ""
        | otherwise                    = "$fun" <> x <> y <> z
    mangle (FunctionTypeParam x y _)   = x <> y
    mangle (Identifier x)              = "@" <> toString x
    mangle (InitializerList x)         = "@" <> x
    mangle (IntAttr x y)               = "@" <> show x <> y
    mangle (Integer x)                 = "$int@" <> show x
    mangle (IntLiteral _ x)            = "@" <> show x
    mangle (Positional _ x)            = x
    mangle (QualifiedIdentifier x)     = x
    mangle (Reference x)               = "$ref" <> x
    mangle (ScopedName _ (Just x) y)   = x <> y
    mangle (ScopedName _ Nothing x)    = x
    mangle (Seq e)                     = concat e
    mangle (StringLiteral x)           = "$stringliteral" <> x
    mangle (TypeIdentifier _ x)        = x
    mangle (Unsigned x)                = "$uint@" <> show x
    mangle Boolean                     = "$bool"
    mangle Float                       = "$float"
    mangle String                      = "$string"
    mangle Void                        = "$void"
    mangle _                           = undefined
