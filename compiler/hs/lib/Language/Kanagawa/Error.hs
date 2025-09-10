{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Error
  ( unexpectedSymbol
  , fancyError
  , warning
  , importFailure
  , parseFileFailure
  , parseFileException
  , desugarError
  , registerFancyError
  , registerSymbolError
  , symbolError
  , symbolLoc
  , makeErrorBundle
  , makeErrorBundles
  , errorMsg
  , errorMsgWithContext
  , warningMsg
  , diagnostics
  , errors
  , warnings
  , ParseError
  , Token
  , ErrorBundle
  , ExtendedError(..)
  , FileException(..)
  ) where

import qualified Control.Exception as Ex
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Ord
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Kanagawa.Desugar
import Language.Kanagawa.Internal
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.Recursion
import Language.Kanagawa.Warning
import Text.Megaparsec
import Text.Megaparsec.Error.Builder

data ExtendedError s =
    ImportError (ErrorBundle s)
    |
    ErrorContext Int (ErrorBundle s)
    |
    FileError String

deriving instance (Eq s, Eq (Token s)) => Eq (ExtendedError s)
deriving instance (Show s, Show (Token s)) => Show (ExtendedError s)

instance (Eq s, Eq (Token s)) => Ord (ExtendedError s) where
    compare (ErrorContext x _) (ErrorContext y _) = compare x y
    compare _ _ = LT

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseError s e)

instance (VisualStream s, TraversableStream s, Eq s, Eq (Token s)) => ShowErrorComponent (ExtendedError s) where
    showErrorComponent (ImportError x) = "In imported file: " ++ errorBundlePretty x
    showErrorComponent (ErrorContext _ x)  = "note: " ++ errorBundlePretty x
    showErrorComponent (FileError x)   = x

newtype FileException = FileException String

instance Ex.Exception FileException

instance Show FileException where
  show (FileException x) = x

type ErrorBundle s = ParseErrorBundle s (ExtendedError s)

diagnostics :: NotedExp n e -> [NotedExp n e]
diagnostics = listWith diag
  where
    diag e@(NotedExp _ ErrorF{}) = [e]
    diag _                       = []

errors :: NotedExp n (ParseError s e) -> [(n, ParseError s e)]
errors = listWith notedError
  where
    notedError (NotedExp n (ErrorF e)) = Just (n, e)
    notedError _ = Nothing

warnings :: Set WarningKind -> NotedExp Src (ParseError s e) -> [(Src, ParseError s e)]
warnings enabled = listWith notedWarning
  where
    notedWarning (NotedExp n (WarningF k w _ ))
        | k `Set.member` enabled = Just (n, w)
    notedWarning _ = Nothing

symbolOffset :: NotedExp Src f -> Int
symbolOffset = offset . note . unfix
  where
    offset (SrcStack a _) = offset a
    offset (Src x _ _ _) = x
    offset SrcUnknown = undefined

symbolLoc :: NotedExp Src f -> String
symbolLoc = loc . note . unfix
  where
    loc (SrcStack a _) = loc a
    loc (Src _ begin _ _) = sourceName begin ++ ":" ++ show (unPos $ sourceLine begin) ++ ":" ++ show (unPos $ sourceColumn begin)
    loc SrcUnknown = undefined

unexpectedSymbol :: (MonadParsec e1 s m) => NotedExp Src e -> String -> m a
unexpectedSymbol u e = parseError $ err (symbolOffset u) (ulabel (getName u) <> elabel e)

fancyError :: Int -> String -> ParseError s e
fancyError offset = errFancy offset . fancy . ErrorFail

importFailure :: MonadParsec (ExtendedError s) s m => Int -> ErrorBundle s -> m a
importFailure offset = parseError . errFancy offset . fancy . ErrorCustom . ImportError

parseFileException :: (MonadParsec (ExtendedError s) s m, Ex.Exception e) => Int -> e -> m a
parseFileException offset = parseFileFailure offset . Ex.displayException

parseFileFailure :: (MonadParsec (ExtendedError s) s m) => Int -> String -> m a
parseFileFailure offset = parseError . errFancy offset . fancy . ErrorCustom . FileError

registerFancyError :: MonadParsec e1 s m => Int -> String -> m ()
registerFancyError offset = registerParseError . fancyError offset

registerSymbolError :: (MonadParsec e1 s m) => NotedExp Src e -> [Char] -> m ()
registerSymbolError e = registerFancyError (symbolOffset e) . (("`" ++ getName e ++ "`: ") ++)

symbolError :: (MonadParsec e1 s m) => NotedExp Src e -> [Char] -> m (NotedExp Src e)
symbolError e m = registerSymbolError e m $> e

desugarError :: String -> ExpF (ParseError s e) a
desugarError = ErrorF . fancyError (-1)

warning :: Int -> WarningKind -> String -> NotedExp n (ParseError s e) -> NotedExp n (ParseError s e)
warning offset kind msg e = copyNote (WarningF kind (fancyError offset message) e) e
  where
    message = "warning: " ++ msg ++ " [--" ++ disableSwitch kind ++ "]"

errorMsg :: String -> NotedExp n (ParseError s e) -> NotedExp n (ParseError s e)
errorMsg = renote . const . desugarError

errorMsgWithContext :: Semigroup n => n -> String -> NotedExp n (ParseError s e) -> NotedExp n (ParseError s e)
errorMsgWithContext n msg (NotedExp n' _) = NotedExp (n' <> n) $ desugarError msg

warningMsg :: WarningKind -> String -> NotedExp n (ParseError s e) -> NotedExp n (ParseError s e)
warningMsg = warning (-1)

makeErrorBundle :: s -> FilePath -> NonEmpty (ParseError s e) -> ParseErrorBundle s e
makeErrorBundle input file xs = ParseErrorBundle
    { bundleErrors = xs
    , bundlePosState = PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos = initialPos file
        , pstateTabWidth = mkPos 4
        , pstateLinePrefix = ""
        }
    }

makeErrorBundles :: (Monad m, Eq s, Eq (Token s)) => (FilePath -> m s) -> [(Src, ParseError s (ExtendedError s))] -> m [ErrorBundle s]
makeErrorBundles getFileContent = mapM (makeErrorBundle' . NE.sortBy (comparing offset)) . NE.groupBy (equating (srcFile . fst))
 where
    makeErrorBundle' xs = do
        let file = srcFile $ fst $ NE.head xs
        input <- getFileContent file
        makeErrorBundle input file <$> mapM fixError xs
      where
        fixError (SrcStack a b, e) = do
            e' <- expandErrorStack 0 e b
            return $ fixErrorOffset (a, e')
        fixError x =
            return $ fixErrorOffset x

        fixErrorOffset (SrcStack a _, e) = fixErrorOffset (a, e)
        fixErrorOffset (Src o _ _ _, FancyError (-1) e) = FancyError o e
        fixErrorOffset x = snd x

    expandErrorStack n e a@Src{} = addErrorContext n e a
    expandErrorStack n e (SrcStack a b) = do
        e' <- addErrorContext n e a
        expandErrorStack (n + 1) e' b
    expandErrorStack _ _ SrcUnknown = undefined

    addErrorContext n (FancyError x y) s@(Src o _ _ _) = do
        e <- makeErrorBundle' $ pure (s, fancyError o "while compiling")
        return $ FancyError x $ Set.insert (ErrorCustom $ ErrorContext n e) y
    addErrorContext _ e _ = return e

    srcFile (SrcStack a _) = srcFile a
    srcFile (Src _ begin _ _) = sourceName begin
    srcFile SrcUnknown = undefined

    offset (SrcStack a _, e) = offset (a, e)
    offset (Src x _ _ _, FancyError (-1) _) = x
    offset (_, FancyError x _) = x
    offset _ = undefined
