{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Syntax.TH
    ( deriveUnidirPatternSyn
    , deriveUnidirPatternSynAndCompletePragma
    , deriveExplBidirPatternSyn
    ) where

import Control.Monad
import Language.Haskell.TH
import Language.Kanagawa.Parser.Syntax (NotedExp, pattern NotedExp, Src(..))

patternSynName :: Name -> Q Name
patternSynName = newName . init . nameBase

patternSynArgNames :: Int -> Q [Name]
patternSynArgNames n = replicateM n (newName "x")

mapConM :: (Traversable t, Monad m) => (Name -> [Type] -> m b) -> t Con -> m (t b)
mapConM fn = mapM con
  where
    con (NormalC name fields) = fn name $ map snd fields
    con (RecC name fields) = fn name $ map (\(_, _, x) -> x) fields
    con _ = undefined

-- Uni-directional synonyms for pattern matching syntax tree ExpF nodes
-- e.g.:
--
--      $(deriveUnidirPatternSyn 'NotedExp ''ExpF)
--
-- generates:
--
--      pattern Alias n a b <- NotedExp _ (AliasF n a b)
--
--      etc...
--
--      {-# COMPLETE
--            Alias,
--            etc...
--      }
--
deriveUnidirPatternSynAndCompletePragma :: Name -> Name -> Q [Dec]
deriveUnidirPatternSynAndCompletePragma f t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t
    syn <- mapConM patternSyn constructors
    return $ map snd syn ++ [PragmaD $ CompleteP (map fst syn) Nothing]
  where
    patternSyn name types = do
        synName <- patternSynName name
        argNames <- patternSynArgNames $ length types
        let argVarPs = map VarP argNames
        return (synName, PatSynD synName (PrefixPatSyn argNames) Unidir (ConP f [WildP, ConP name argVarPs]))

-- Same as deriveUnidirPatternSynAndCompletePragma but ommits the COMPLETE pragma
deriveUnidirPatternSyn :: Name -> Name -> Q [Dec]
deriveUnidirPatternSyn f t = init <$> deriveUnidirPatternSynAndCompletePragma f t

-- Explicit bi-directional patterns that can be used for pattern matching
-- (ignoring annotation) and in expressions (setting annotation to SrcUnknown
-- e.g.:
--
--      $(deriveExplBidirPatternSyn ''ExpF)
--
-- generates:
--
--      pattern Alias :: QualifiedName -> NotedExp Src e -> NotedExp Src e -> NotedExp Src e
--      pattern Alias n a b <- NotedExp _ (AliasF n a b)
--        where Alias n a b = NotedExp SrcUnknown (AliasF n a b)
--
--      etc...
--
deriveExplBidirPatternSyn :: Name -> Q [Dec]
deriveExplBidirPatternSyn t = do
    TyConI (DataD _ _ [KindedTV e _, KindedTV a _] _ constructors _) <- reify t
    concat <$> mapConM (patternSyn e a) constructors
  where
    patternSyn e a name types = do
        synName <- patternSynName name
        argNames <- patternSynArgNames $ length types
        let argVarPs = map VarP argNames
            argVarEs = map VarE argNames
            notedExpT = AppT (AppT (ConT ''NotedExp) (ConT ''Src)) (VarT e)
            notedExpE = AppE (AppE (ConE 'NotedExp) (ConE 'SrcUnknown))
            typ = \case
                (AppT t1 t2)      -> AppT (typ t1) (typ t2)
                (VarT n) | n == a -> notedExpT
                x                 -> x
            arrow = AppT . AppT ArrowT . typ
            sig = PatSynSigD synName $ ForallT [] [] $ ForallT [] [] $ foldr arrow notedExpT types
            pat = PatSynD synName (PrefixPatSyn argNames)
                        (ExplBidir [Clause argVarPs (NormalB (notedExpE (foldl AppE (ConE name) argVarEs))) []])
                        (ConP 'NotedExp [WildP, ConP name argVarPs])
        return [sig, pat]
