{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Recursion
  ( -- * Fixed points
    Fix(..)
  , unfix
  -- Algebras
  , Algebra
  , AlgebraM
  , RAlgebra
  , RAlgebraM
  , CVAlgebra
  -- Morphisms
  , cata
  , para
  , histo
  , Attributed(..)
  , cataM
  , paraM
  , mapWith
  , mapUniqueWith
  , setWith
  , listWith
  ) where

import Control.Arrow
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter

newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)

type Algebra f a = f a -> a
type AlgebraM t m a = t a -> m a
type RAlgebra f a = Fix f -> f a -> a
type RAlgebraM t m a = Fix t -> t a -> m a
type CVAlgebra f a = f (Attributed f a) -> a

data Attributed f a =
        Attributed
        { attribute :: a
        , prev      :: f (Attributed f a)
        }

-- BUGBUG: The instance doesn't type-check unless annotation is removed.
-- We don't use annotation currently so going with this workaround since
-- using `pretty` in pretty printer definition reads better than `attribute`.
instance Pretty (Attributed f (Doc ann)) where
    pretty = unAnnotate . attribute

histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo alg = distHisto >>> attribute where
  distHisto = unfix >>> fmap distHisto >>> (alg &&& id) >>> mkAttr
  mkAttr (a, b) = Attributed a b

para :: Functor f => RAlgebra f a -> Fix f -> a
para alg t = alg t $ para alg <$> unfix t

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = para (const alg)

paraM :: (Applicative m, Monad m, Traversable t) => RAlgebraM t m a -> Fix t -> m a
paraM alg t = alg t =<< traverse (paraM alg) (unfix t)

cataM :: (Applicative m, Monad m, Traversable t) => AlgebraM t m a -> Fix t -> m a
cataM alg = paraM (const alg)

-- Given a function that extracts key-value pair(s) from a fixed point of a functor
-- generate a Map k v. Values for conflicting keys are combined with mappend.
mapWith :: (Ord k, Functor f, Foldable f, Foldable t, Semigroup a) => (Fix f -> t (k, a)) -> Fix f -> Map k a
mapWith f = para insert
  where
    insert e x = foldr (uncurry $ Map.insertWith (flip (<>))) (Map.unionsWith (<>) x) (f e)

-- Given a function that extracts key-value pairs from a fixed point of a functor
-- generate a Map k v. In case of conflicting key the first value wins.
mapUniqueWith :: (Ord k, Functor f, Foldable f, Foldable t) => (Fix f -> t (k, a)) -> Fix f -> Map k a
mapUniqueWith f = para insert
  where
    insert e x = foldr (uncurry Map.insert) (Map.unions x) (f e)

-- Given a function that extracts value(s) from a fixed point of a functor
-- generate a Set of extracted values
setWith :: (Functor f, Foldable f, Foldable t, Ord a) => (Fix f -> t a) -> Fix f -> Set a
setWith f = para insert
  where
    insert e x = foldr Set.insert (Set.unions x) (f e)

-- Given a function that extracts value(s) from a fixed point of a functor
-- generate result by lifting all extracted values and combining them with mappend.
listWith :: (Applicative f, Foldable t1, Monoid (f a), Functor t1, Foldable t2) => (Fix t1 -> t2 a) -> Fix t1 -> f a
listWith f = para append
  where
    append e x = foldr ((<>) . pure) (fold x) (f e)
