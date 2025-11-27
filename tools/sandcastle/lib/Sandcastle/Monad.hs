{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle.Monad
  ( Env(..)
  , setModuleIdentifier
  , S(..)
  , getSvgIdx
  , App(..)
  , runApp
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Set (Set)
import Language.Kanagawa.Parser.SymbolTable hiding (symbols)
import Language.Kanagawa.Parser.Syntax
import Sandcastle.Cli (Cli)
import qualified Sandcastle.Cli as Cli
import System.FilePath

data Env = Env
    { format           :: Cli.Format
    , fileExt          :: String   -- ^ File format extension
    , moduleIdentifier :: [String]
    , outputDirectory  :: [FilePath]
    , sourceUrl        :: String
    , svgbob           :: Bool     -- ^ Generate Svgbob diagrams
    , anchors          :: Set QualifiedName
    , symbols          :: Symbols
    }
  deriving (Eq, Show)

initEnv
  :: String
  -> Cli
  -> Set QualifiedName
  -> Symbols
  -> Env
initEnv fExt cli ancs syms = Env
  { format           = Cli.format cli
  , fileExt          = fExt
  , moduleIdentifier = mempty
  , outputDirectory  = splitPath $ Cli.output cli
  , sourceUrl        = Cli.source_url cli
  , svgbob           = Cli.svgbob cli
  , anchors          = ancs
  , symbols          = syms
  }

setModuleIdentifier :: [String] -> Env -> Env
setModuleIdentifier ss env = env{ moduleIdentifier = ss }

newtype S = S{ svgIdx :: Int }
  deriving (Eq, Num, Read, Show)

-- | Get the current svg index and increment state.
getSvgIdx :: MonadState S m => m Int
getSvgIdx = do
  i <- gets svgIdx
  modify $ \s -> s{ svgIdx = i + 1 }
  return i

newtype App a = App{ unApp :: ReaderT Env (StateT S IO) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadReader Env, MonadState S
             )

instance Semigroup a => Semigroup (App a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (App a) where
  mempty = return mempty

runApp
  :: String -- ^ file extension
  -> Cli
  -> Set QualifiedName -- ^ anchors
  -> Symbols
  -> App a
  -> IO a
runApp fExt cli ancs syms =
  flip evalStateT 0 . flip runReaderT (initEnv fExt cli ancs syms) . unApp
