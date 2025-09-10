{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Type.Inference
    ( inferType
    , inferType'
    ) where

import Language.Kanagawa.Constexpr
import Language.Kanagawa.Desugar
import Language.Kanagawa.Error
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Language.Kanagawa.Type

inferType'
    :: SymbolMap (Typed n) (ParseError s e)
    -> NotedExp (Typed n) (ParseError s e)
    -> NotedExp (Typed n) (ParseError s e)
inferType' symbols = cata infer
  where
    infer = inferExpType symbols . unfix . evaluateConstexpr symbols

inferType
    :: [NotedExp (Typed n) (ParseError s e)]
    -> [NotedExp (Typed n) (ParseError s e)]
inferType =
    loop []
  where
    loop prevSig p
        | prevSig == sig = inferred
        | otherwise      = loop sig inferred
      where
        symbols = getSymbols' TemplateDecl p
        inferred = parallel (inferExpTree symbols) p
        -- Count of unresolved types is monotonically decreasing
        sig :: [Integer]
        sig = parallel (para go) inferred
          where
            go e
                | typeResolved e = sum
                | otherwise      = succ . sum

inferExpTree :: SymbolMap (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e) -> NotedExp (Typed n) (ParseError s e)
inferExpTree symbols = inferExpTree' infer
  where
    infer = staticControl . unfix . evaluateConstexpr symbols . unfix . inferExpType symbols

    inferExpTree' alg = alg . go
      where
        -- Postpone for subtrees that need to be desugared
        go (NotedExp n e@LambdaF{})                          = Note n e
        go (NotedExp n e@FunctionF{..})
            | any (tIsFunction . typeOf) $ unfix funcParams' = Note n e
            | not $ all typeResolved $ unfix funcParams'     = Note n $ e { funcParams = funcParams' }
            | isLocalScope e                                 = Note n $ e { funcParams = funcParams', funcReturnType = funcReturnType' }
          where
            funcParams' = inferExpTree' alg funcParams
            funcReturnType' = inferExpTree' alg funcReturnType
        go (NotedExp n (TemplateF a b))                      = Note n (TemplateF (inferExpTree' alg <$> a) b)
        go (NotedExp n (StaticIfF a b c))                    = Note n (StaticIfF (inferExpTree' alg a) b c)
        -- Evaluate static control flow within local classes
        go e@(NotedExp _ ClassF{})
              | isLocalScope e                               = inferExpTree' staticControl <$> unfix e
        go e                                                 = inferExpTree' alg <$> unfix e

