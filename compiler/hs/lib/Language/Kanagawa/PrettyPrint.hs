{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.PrettyPrint (
      prettyExp
    , prettySource
    ) where

import Prettyprinter hiding (tupled, braces, hsep)
import qualified Prettyprinter as PP
import Language.Kanagawa.Desugar
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.Pattern
import Language.Kanagawa.Recursion

prettyExp :: (Functor f, Show (f (Doc ann))) => Fix (Note Src f) -> Doc ann
prettyExp = cata $ nest 4 . (line <>) . withDocs
  where
    withDocs (Note n x) = case n of
        Src{ docs = ([], []) } -> viaShow x
        Src{ docs = (pre, post) } -> mconcat
            [ dquotes $ vsep $ pretty <$> pre ++ post
            , line
            , viaShow x
            ]
        _ -> viaShow x

instance Pretty (NotedExp n e) where
    pretty = prettySource 4

prettySource :: Int -> NotedExp n e -> Doc ann
prettySource n = histo src . desugar removeNestedSeq
  where
    braces           = PP.braces . (<> line) . nest n
    braces'          = PP.braces . nest n
    commaSepList     = (nest n . group . (line' <>) . vsep . punctuate comma) . map pretty
    commaLineSepList = (vcat . punctuate comma) . map pretty
    tupledList       = parens . commaSepList
    arrayDimList     = encloseSep lbracket rbracket (rbracket <> lbracket) . map pretty
    statementList    = braces . hcat . map ((line <>) . statement)
    fieldsList       = braces . hcat . map ((line <>) . semiTerm)
    attributeList    = brackets . brackets . commaSepList
    moduleName       = hcat . punctuate dot . map pretty

    fromSeq f        = from . prev
      where
        from (Seq x) = f x
        from  _      = undefined

    tupled           = fromSeq tupledList
    commaSep         = fromSeq commaSepList
    commaLineSep     = fromSeq commaLineSepList
    arrayDim         = fromSeq arrayDimList
    statements       = fromSeq statementList
    fields           = fromSeq fieldsList

    attributes       = from . prev
      where
        from (Seq []) = emptyDoc
        from (Seq x)  = attributeList x <> space
        from _        = undefined

    src (Alias _ a b)                       = "using" <+> pretty b <+> equals <+> pretty a
    src (Annotated a b)
        | isAtomic $ prev a                 = "atomic" <+> statement b
        | otherwise                         = attributes a <> statement b
    src (Array a b c)                       = attributes a <> pretty b <> arrayDim c
    src (ArrayAccess a b)                   = pretty a <> brackets (pretty b)
    src (ArrayAccessLValue a b)             = pretty a <> brackets (pretty b)
    src (Assign a b)                        = pretty a <+> equals <> nest n (softline <> pretty b)
    src Auto                                = "auto"
    src Barrier                             = "barier"
    src (Binary LutMul e1 e2)               = "lutmul" <> parens (pretty e1 <> comma <+> pretty e2)
    src (Binary op e1 e2)                   = parens (hang 0 (pretty e1 <> softline <> pretty op <+> pretty e2))
    src (BitOffset a b)                     = "bitoffset" <> parens (pretty a <> comma <+> pretty b)
    src (BoolLiteral True)                  = "true"
    src (BoolLiteral False)                 = "false"
    src Boolean                             = "bool"
    src (ByteOffset a b)                    = "byteoffset" <> parens (pretty a <> comma <+> pretty b)
    src (Cast a b)                          = "cast" <> angles (pretty a) <> parens (pretty b)
    src (Capture _ a)                       = pretty a
    src (Class _ a b)                       = "class" <+> pretty a <> line <> braces (pretty b) <> line
    src (Decltype _ a)                      = "decltype" <> parens (pretty a)
    src (Concat a)                          = "concat" <> tupled a
    src (Const a)                           = "const" <+> pretty a
    src (Designator a b)                    = dot <> pretty a <+> equals <+> pretty b
    src (DefaultInitialization a)           = "default" <+> equals <+> pretty a
    src (DoWhile a b c)                     = attributes a <> "do" <+> statement c <+> "while" <> parens (pretty b) <> line
    src (Enum _ a b c)                      = "enum" <+> pretty a <+> colon <+> pretty b <> line <> braces (line <> commaLineSep c) <> line
    src (EnumConstant _ a Nothing)          = pretty a
    src (EnumConstant _ a (Just b))         = pretty a <+> equals <+> pretty b
    src (EnumValue a _)                     = pretty a
    src Error{}                             = undefined
    src (ExportType a b)                    = attributes a <> "export" <+> pretty b
    src (Extern a b)                        = attributes a <> "extern" <+> pretty b
    src (FanOut a b)                        = "fan_out" <> angles (pretty a) <> parens (pretty b)
    src (FlagAttr x)                        = pretty x
    src Float                               = "float32"
    src (FloatLiteral x)                    = pretty x
    src (FuncParam _ a b c)                 = attributes a <> pretty b <+> pretty c
    src (Function _ _ a b c d e f)          = attributes a <> pretty b <> pretty c <> softline <> pretty d <> tupled e <> line <> statements f <> line
    src (FunctionDecl _ a b c d)            = attributes a <> pretty b <> softline <> pretty c <> tupled d
    src (FunctionCall a b c)                = attributes a <> pretty b <> tupled c
    src (FunctionModifier x)                = pretty x <> space
    src (FunctionSpecifier Nothing b)       = pretty b
    src (FunctionSpecifier (Just a) b)      = pretty a <> dot <> pretty b
    src (FunctionType a b c)                = attributes a <> tupled b <+> "->" <+> pretty c
    src (FunctionTypeParam a b Nothing)     = attributes a <> pretty b
    src (FunctionTypeParam a b (Just c))    = attributes a <> pretty b <+> pretty c
    src (Lambda _ _ a b c d)                = brackets (commaSep a) <> tupled b <+> "->" <+> pretty c <> line <> statements d
    src (Identifier x)                      = pretty x
    src (If a b Nothing)                    = "if" <+> parens (nest n (pretty a)) <+> statement b <> line
    src (If a b (Just c))                   = "if" <+> parens (nest n (pretty a)) <+> statement b <+> "else" <+> statement c <> line
    src (StaticIf a b Nothing)              = "static if" <+> parens (nest n (pretty a)) <+> statement b <> line
    src (StaticIf a b (Just c))             = "static if" <+> parens (nest n (pretty a)) <+> statement b <+> "else" <+> statement c <> line
    src (InitializerList a)                 = braces' (commaSep a)
    src (IntAttr x a)                       = pretty x <> parens (pretty a)
    src (IntLiteral (Just w) x)             = pretty x <> pretty w
    src (IntLiteral Nothing x)              = pretty x
    src (Integer x)                         = "int" <> pretty x
    src (InterpolatedString a)              = dquotes $ hcat $ map stringSegment a
    src (MemberAccess a b)                  = pretty a <> dot <> pretty b
    src (ModuleDecl a b c)                  = pretty a <> line <> braces (line <> commaLineSepList b) <> line <> pretty c
    src (ModuleDiff a b)                    = "module" <+> moduleName a <+> "\\" <+> moduleName b
    src (ModuleIdentifier a)                = "module" <+> moduleName a
    src (Mux a b)                           = "mux" <> parens (pretty a <> comma <+> commaSep b)
    src (NamedValue a)                      = pretty a
    src (NestedScope _ _ a)                 = statements a
    src (ParamInt e)                        = "int" <> angles (pretty e)
    src (ParamUint e)                       = "uint" <> angles (pretty e)
    src (Positional _ a)                    = pretty a
    src Private                             = "private:"
    src Public                              = "public:"
    src (RangeFor a b c d)                  = attributes a <> "for" <+> parens (pretty b <+> colon <+> pretty c) <+> statement d <> line
    src (Reorder a)                         = "reorder" <+> statement a
    src (Return a)                          = "return" <+> pretty a
    src (Reference a)                       = pretty a <> "&"
    src (QualifiedIdentifier a)             = pretty a
    src (ScopedName _ Nothing a)            = pretty a
    src (ScopedName _ (Just a) b)
        | isInternalQualifier $ prev a      = pretty b
        | otherwise                         = pretty a <> "::" <> pretty b
    src (Seq x)                             = vcat (map ((line <>) . semiTerm) x)
    src (Static a)                          = "static" <> parens (pretty a)
    src (StaticAssert a)                    = "static assert" <> parens (pretty a)
    src (StaticAssertLegacy a)              = "static_assert" <> parens (pretty a)
    src String                              = "string"
    src (StringLiteral s)                   = dquotes (pretty s)
    src (Struct _ a b)                      = "struct" <+> pretty a <> line <> fields b <> line
    src (Switch a b)                        = "switch" <> parens (pretty a) <> line <> braces (pretty b) <> line
    src (SwitchCase a b)                    = nest n ("case" <+> pretty a <> colon <> line <> statements b <> line <> "break" <> semi)
    src (SwitchDefault a)                   = nest n ("default" <> colon <> line <> statements a <> line <> "break" <> semi)
    src (Template a b)                      = "template" <+> angles (commaSepList a) <> line <> pretty b
    src (TemplateInstance a b)
        | null $ prev b                     = pretty a
    src (TemplateInstance a b)              = pretty a <> angles (commaSep b)
    src (TemplateParam _ a b _ Nothing)     = pretty a <+> pretty b
    src (TemplateParam _ a b _ (Just c))    = pretty a <+> pretty b <+> equals <+> pretty c
    src This{}                              = "this"
    src (Attr x a)                          = pretty x <> parens (pretty a)
    src (TypeIdentifier _ a)                = pretty a
    src Typename                            = "typename"
    src (TypeParam _ _ a _)                 = pretty a
    src (Unary op@BitSizeof e)              = pretty op <+> pretty e
    src (Unary op@ByteSizeof e)             = pretty op <+> pretty e
    src (Unary op@Clog2 e)                  = pretty op <> parens (pretty e)
    src (Unary op e)                        = pretty op <> pretty e
    src (Union _ a b)                       = "union" <+> pretty a <> line <> fields b <> line
    src (StaticFor a b c)                   = "static for" <> parens (hang 0 (pretty a <+> colon <+> pretty b)) <+> statement c <> line
    src (UnrolledFor a b c)                 = "static for" <> parens (hang 0 (pretty a <+> colon <+> pretty b)) <+> statement c <> line
    src (Unsigned x)                        = "uint" <> pretty x
    src (Variable _ a b c Nothing m)        = attributes a <> static m <> pretty b <+> pretty c
    src (Variable _ a b c (Just d) m)       = attributes a <> static m <> pretty b <+> pretty c <+> equals <+> nest n (pretty d)
    src Void                                = "void"
    src (Warning _ _ a)                     = pretty a

    stringSegment (str, Nothing)            = pretty str
    stringSegment (str, Just (e, ne, p, f)) = pretty str <> PP.braces (pretty e <> nameEqual ne <> precision p <> pretty f)
      where
        nameEqual True = "="
        nameEqual False = mempty
        precision (Just x) = comma <> pretty x
        precision Nothing = mempty

    isInternalQualifier (ScopedName _ Nothing e) = isInternalQualifier $ prev e
    isInternalQualifier (Identifier x)
        | ('@':_) <- toString x                   = True
        | unresolvedName == x                     = True
    isInternalQualifier _                         = False

    static DeclareStatic = "static" <> space
    static _ = ""

    isAtomic (Seq [e])
        | (FlagAttr Atomic) <- prev e = True
    isAtomic _                        = False

    statement e = case prev e of
        DefaultInitialization{} -> "static" <+> semiTerm e
        _ -> semiTerm e

    semiTerm e = pretty e <> term (prev e)
      where
        term = \case
            Annotated{}     -> mempty
            Class{}         -> mempty
            DoWhile{}       -> mempty
            Enum{}          -> mempty
            Function{}      -> mempty
            If{}            -> mempty
            ModuleDecl{}    -> mempty
            NestedScope{}   -> mempty
            Private{}       -> mempty
            Public{}        -> mempty
            RangeFor{}      -> mempty
            Reorder{}       -> mempty
            StaticFor{}     -> mempty
            StaticIf{}      -> mempty
            Struct{}        -> mempty
            Switch{}        -> mempty
            (Template _ a)  -> term $ prev a
            Union{}         -> mempty
            UnrolledFor{}   -> mempty
            (Variable _ _ _ _ _ DeclareUninitConst)
                            -> mempty
            _               -> semi
