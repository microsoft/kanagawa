{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Template
    ( instantiateTemplates
    ) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Language.Kanagawa.Closure
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Function
import Language.Kanagawa.Internal
import Language.Kanagawa.Mangle
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Language.Kanagawa.Type
import Language.Kanagawa.Type.Inference
import Language.Kanagawa.TypeCheck

{-- Rewrite syntax tree to instantiate templates.

    Replace TemplateF nodes with SeqF of zero or more instances, decorating
    the identifier with instance arguments and replacing TypeParamF nodes
    within the template definition with corresponding instance arguments.

    Replace TemplateInstanceF nodes with decorated named identifying the
    instance.

    Matching of instance arguments to template parameters is done by index
    and declaration scope.

    Since templates can be nested, instantiation is performed recursively until
    all instances are resolved.

--}

instantiateTemplates
    :: Semigroup n
    => (Ord (Token s), Ord s, Ord e)
    => Int
    -> Int
    -> ([NotedExp (Typed n) (ParseError s e)], Bool)
    -> ([NotedExp (Typed n) (ParseError s e)], Bool)
instantiateTemplates templatePasses templateIterations (program, hasErrors)
    -- recursively instantiate templates until all instances are resolved
    | hasErrors = (program, hasErrors)
    | hasErrors' = (program', hasErrors')
    | templateIterations == 0 || Map.null decidedInstances = (parallel (desugar deduplicateInstances) program', False)
    | otherwise = instantiateTemplates templatePasses (templateIterations - 1) (inferType $ parallel (desugar (instantiate . resolve)) program', False)
  where
    program' = foldr ($) program passes'

    passes'
        | templateIterations == 0 = drop (length passes - templatePasses) passes
        | otherwise = passes

    withSymbols fn p = fn (getSymbols p) p

    withSymbols' config fn p = fn (getSymbols' config p) p

    -- desugaring passes before template instantiation
    passes = [ withSymbols $ parallel . templateTypeCheck
             ,               inferType
             , withSymbols $ parallel . deduceTemplateArgs
             ,               parallel $ desugar closureCalls
             ,               inferType
             ,               parallel $ moveFunctions
             ,               parallel $ desugarShallow' abbreviatedFunctionTemplate
             ,               parallel $ desugarLambdas
             , withSymbols' NoTemplate $
                             parallel . desugar . addLambdaWrappers
             ]

    symbols = getSymbols program'
    hasErrors' = hasErrorSymbol symbols

    -- All template instances that can be instantiated, i.e. have all arguments resolved.
    -- Instances within a body of another template are postponed until after that template is instantiated.
    decidedInstances = Map.unionsWith Map.union $ parallelChunksOf 4 (Map.unionsWith Map.union) $ parallel (para decided) program'
      where
        decided e@(TemplateInstance a b@(Seq args)) x
            | all Map.null x && templateResolved symbols template b =
                Map.singleton templateQualifiedName directInstance
            | templateArgsResolved template args =
                Map.unionsWith Map.union $ Map.unionsWith Map.union x : indirectInstances template
          where
            directInstance = Map.singleton b $ note $ unfix e

            indirectInstances (Just t@(Template params _)) =
                map (para decided . resolveTemplateParam symbols t args) params
            indirectInstances _ = mempty

            template = lookupSymbol a symbols
            templateQualifiedName = getQualifiedName $ fromJust template

        decided _ (Note _ StaticIfF{..})  = staticIfCond
        decided _ (Note _ StaticForF{..}) = forLimit
        decided _ (Note _ TemplateF{..})  = Map.unionsWith Map.union templateParams
        decided _ x                       = Map.unionsWith Map.union x

    -- Replace template definition with a SeqF that contains the template
    -- as well as instantiations of the template for decided instances.
    instantiate t@TemplateF{..}
        | Typename <- templateDecl = t
        | null templateInstances = t
        | otherwise = SeqF $ map makeInstance templateInstances ++ [copyNote t templateDecl]
      where
        templateQualifiedName = getQualifiedName t

        -- List of all instances for this template
        templateInstances = Map.toList $ Map.findWithDefault Map.empty templateQualifiedName decidedInstances

        -- Create an instance of the template for a given set of arguments
        -- by decorating the declName identifier and replacing occurrences of
        -- TypeParam with corresponding arguments.
        makeInstance (args@(Seq args'), n) = setTypeOfInstance templateQualifiedName (zip (map getName templateParams) args')
                               $ desugar (modifyDeclScopeName mangleScope)
                               $ desugarWithNote n rewrite instanceDecl
          where
            instanceName = getName instanceDecl
            instanceDecl = renote mangleDeclName templateDecl
            mangleScope x | templateQualifiedName `isPrefixOf` x = setAt (length templateQualifiedName - 1) instanceName x
            mangleScope x = x
            rewrite (TemplateInstance a _) (TemplateInstanceF _ b)
                | getQualifiedName a == templateQualifiedName = TemplateInstanceF a b
            rewrite _ x@(ScopedNameF s a b)
                | getQualifiedName x == templateQualifiedName = mangleTemplateScopedName t args x
                | getQualifier x == templateQualifiedName     = ScopedNameF s (renote (mangleTemplateScopedName t args) <$> a) b
            rewrite _ x                                       = resolveTypeParam t args' x
            mangleDeclName x = x { declName = mangleTemplateIdentifier t args $ declName x }
        makeInstance _ = undefined 

    instantiate x = deduplicateInstances $ removeNestedSeq x

    deduplicateInstances (SeqF a) = SeqF (nubBy dup a)
      where
        dup (NotedExp _ t1) (NotedExp _ t2) =
            userDefinedTypes t1 t2
         && getQualifiedName t1 == (getQualifiedName t2 :: [String])
        userDefinedTypes ClassF{} ClassF{} = True
        userDefinedTypes StructF{} StructF{} = True
        userDefinedTypes UnionF{} UnionF{} = True
        userDefinedTypes AliasF{} AliasF{} = True
        userDefinedTypes TemplateF{} TemplateF{} = True
        userDefinedTypes FunctionF{} FunctionF{} = True
        userDefinedTypes _ _ = False
    deduplicateInstances x = x

    -- Replace TemplateInstance node with decorated name identifying the instance.
    resolve (TemplateInstanceF a b) | b `Map.member` Map.findWithDefault Map.empty templateQualifiedName decidedInstances =
        mangleTemplateScopedName (unNote $ unfix $ fromJust template) b $ unNote $ unfix a
      where
        template = lookupSymbol a symbols
        templateQualifiedName = maybe [] getQualifiedName template
    resolve e = e
