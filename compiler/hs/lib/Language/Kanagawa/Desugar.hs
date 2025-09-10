{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Desugar
    ( DesugarAlgebra
    , scopedQualifier
    , setQualifier
    , addQualifierFrom
    , makeQualified
    , suffixName
    , indexedName
    , indexedIdentifier
    , indetifierAppend
    , functionSpecifier
    , freeFunctionSpecifier
    , typeIdentifier
    , functionScope
    , isLocalScope
    , desugar
    , desugar'
    , desugarShallow
    , desugarShallow'
    , desugarM
    , desugarWithNote
    , noteContext
    , removeNestedSeq
    , removeNestedTypeIdentifier
    , renote
    , copyNote
    , MemberAccess(..)
    , mapClassMembers
    , modifyDeclScopeName
    , cons
    , parallel
    , parallelChunksOf
    , (.:)
    ) where

import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import Data.String
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion

(.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:) = (.) . (.)

infixr 9 .:

parallel :: (a -> b) -> [a] -> [b]
parallel = parMap rpar

parallelChunksOf :: Int -> ([a] -> b) -> [a] -> [b]
parallelChunksOf size f = parallel f . chunksOf size
  where
    chunksOf _ [] = []
    chunksOf n xs = as : chunksOf n bs where (as,bs) = splitAt n xs

-- cata wrapper that simplifies rewriting AST nodes while preserving annotations
type DesugarAlgebra n e = Exp n e -> Exp n e

desugar :: DesugarAlgebra n e -> NotedExp n e -> NotedExp n e
desugar f = cata f'
  where
    f' (Note n e) = NotedExp n (f e)

desugar' :: (n -> DesugarAlgebra n e) -> NotedExp n e -> NotedExp n e
desugar' f = cata f'
  where
    f' (Note n e) = NotedExp n (f n e)

desugarShallow :: (Exp n e -> Maybe (Exp n e)) -> NotedExp n e -> NotedExp n e
desugarShallow f = para f'
  where
    f' e' (Note n e) = maybe e' (NotedExp n) (f e)

desugarShallow' :: (n -> Exp n e -> Maybe (Exp n e)) -> NotedExp n e -> NotedExp n e
desugarShallow' f = para f'
  where
    f' e' (Note n e) = maybe e' (NotedExp n) (f n e)

desugarM :: (Monad m) => (Exp n e -> m (Exp n e)) -> NotedExp n e -> m (NotedExp n e)
desugarM f = cataM f'
  where
    f' (Note n e) = NotedExp n <$> f e

desugarWithNote :: Semigroup n => n -> (NotedExp n e -> Exp n e -> Exp n e) -> NotedExp n e -> NotedExp n e
desugarWithNote n f = para f'
  where
    f' e' (Note n' e) = NotedExp (n' <> n) (f e' e)

-- combinator to update an expression while keeping the note
renote :: (Exp n e -> Exp n e) -> NotedExp n e -> NotedExp n e
renote f (NotedExp n e) = NotedExp n (f e)

noteContext :: Semigroup n => NotedExp n e -> NotedExp n e -> NotedExp n e
noteContext (NotedExp n _) (NotedExp n' e) = NotedExp (n' <> n) e

copyNote :: Exp n e -> NotedExp n e -> NotedExp n e
copyNote = renote . const

cons :: NotedExp n e -> NotedExp n e -> NotedExp n e
cons x = renote cons'
  where
    cons' (SeqF xs) = SeqF $ x : xs
    cons' _ = undefined

data MemberAccess = PublicMember | PrivateMember
    deriving (Eq, Show)

mapClassMembers :: Traversable t => (MemberAccess -> NotedExp n e -> b) -> t (NotedExp n e) -> t b
mapClassMembers f = snd . mapAccumL member PrivateMember
  where
    member m a = case a of
        Public  -> (PublicMember, f m a)
        Private -> (PrivateMember, f m a)
        _       -> (m, f m a)

removeNestedTypeIdentifier :: DesugarAlgebra n e
removeNestedTypeIdentifier = rewrite
  where
    rewrite (TypeIdentifierF s (TypeIdentifier _ a))      = TypeIdentifierF s a
    rewrite (TemplateInstanceF (TypeIdentifier _ a) b)    = TemplateInstanceF a b
    rewrite (ScopedNameF s (Just (TypeIdentifier _ a)) b) = ScopedNameF s (Just a) b
    rewrite e                                             = e

-- remove extra nested Seq nodes created during desugaring
removeNestedSeq :: DesugarAlgebra n e
removeNestedSeq (SeqF a) = SeqF (foldMap toList' a)
  where
    toList' (NotedExp _ (SeqF x)) = x
    toList' x = [x]
removeNestedSeq x = x

modifyDeclScopeName :: (QualifiedName -> QualifiedName) -> DesugarAlgebra n e
modifyDeclScopeName fn = rewrite
  where
    update e = e { declScopeName = fn $ declScopeName e }

    rewrite e@CaptureF{}            = update e
    rewrite e@FuncParamF{}          = update e
    rewrite e@StructF{}             = update e
    rewrite e@EnumConstantF{}       = update e
    rewrite e@EnumF{}               = update e
    rewrite e@UnionF{}              = update e
    rewrite e@AliasF{}              = update e
    rewrite e@ClassF{}              = update e
    rewrite e@VariableF{}           = update e
    rewrite e@FunctionF{}           = update e
    rewrite e@ThisF{}               = update e
    rewrite e@NestedScopeF{}        = update e
    rewrite e@DecltypeF{}           = update e
    rewrite e@LambdaF{}             = update e
    rewrite e@FunctionDeclF{}       = update e
    rewrite (TypeIdentifierF s a)   = TypeIdentifierF (fn s) a
    rewrite (TypeParamF s a b c)    = TypeParamF (fn s) a b c
    rewrite (ScopedNameF s a b)     = ScopedNameF (fn s) a b
    rewrite e                       = e

suffixName :: String -> Name -> Name
suffixName x = mapName (<> x)

indexedName :: Int -> String -> Name
indexedName x = fromString . (++ show x)

indexedIdentifier :: Int -> String -> Exp n e
indexedIdentifier = IdentifierF .: indexedName

functionScope :: QualifiedName -> QualifiedName
functionScope = (++ ["$"])

isLocalScope :: ExpLike a => a -> Bool
isLocalScope = go . getScope
  where
    go [] = False
    go xs = '$' == head (toString $ last xs)

scopedQualifier :: Foldable t => QualifiedName -> NotedExp n e -> t Name -> Maybe (NotedExp n e)
scopedQualifier n e = foldl' scopedName Nothing
  where
    scopedName x y = Just (copyNote (ScopedNameF n x (copyNote (IdentifierF y) e)) e)

addQualifierFrom :: ExpLike a => a ->  Exp n e -> Exp n e
addQualifierFrom = setQualifier . getQualifier

makeQualified :: ExpLike a => a -> NotedExp n e -> NotedExp n e
makeQualified = renote . addQualifierFrom

setQualifier :: QualifiedName -> Exp n e -> Exp n e
setQualifier [] e                               = e
setQualifier qualifier (ScopedNameF n (Just a) b)
    | getName a == last qualifier               = ScopedNameF n (Just $ renote (setQualifier $ init qualifier) a) b
setQualifier qualifier (ScopedNameF n _ b)      = ScopedNameF n (scopedQualifier n b qualifier) b
setQualifier qualifier (TemplateInstanceF a b)  = TemplateInstanceF (renote (setQualifier qualifier) a) b
setQualifier qualifier (QualifiedIdentifierF a) = QualifiedIdentifierF (renote (setQualifier qualifier) a)
setQualifier _ e = e

indetifierAppend :: NotedExp n e -> String -> NotedExp n e
indetifierAppend e@(Identifier x) y = copyNote (IdentifierF $ suffixName y x) e
indetifierAppend _ _ = undefined

typeIdentifier :: QualifiedName -> QualifiedName -> NotedExp n e -> NotedExp n e
typeIdentifier declScopeName name e = copyNote (TypeIdentifierF declScopeName $ fromJust $ scopedQualifier declScopeName e name) e

functionSpecifier :: Maybe (NotedExp n e) -> NotedExp n e -> NotedExp n e
functionSpecifier o f = copyNote (FunctionSpecifierF o f) f

freeFunctionSpecifier :: NotedExp n e -> NotedExp n e
freeFunctionSpecifier = functionSpecifier Nothing
