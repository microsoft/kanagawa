{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle
    ( Cli(..)
    , Format(..)
    , app
    ) where

import Control.Monad.Extra
import Control.Monad.State
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter.Render.Text (putDoc)
import Language.Kanagawa.Parser.IO (exitErrors, parseProgram)
import Language.Kanagawa.PrettyPrint (prettyExp)
import Language.Kanagawa.Symbols
import Sandcastle.Cli
import Sandcastle.DocGen
import System.Directory
import System.FilePath
import System.FilePattern
import System.FilePattern.Directory
import Text.Pandoc hiding (Format)
import Text.Pandoc.Builder hiding (Format)
import qualified Text.Pandoc.UTF8 as UTF8 (writeFile)
import Text.Pretty.Simple (pPrint)

app :: Cli -> IO ()
app =
  withFilePatterns $
  withTargetDevice $ \cli -> do
    -- Parse
    parsedFiles <- flip execStateT [] $ parseProgram $ cliToParseOptions cli
    let (errs, results) = partitionEithers $ sequence <$> parsedFiles
    unless (null errs) $ exitErrors errs

    -- Generate
    let fmt = format cli
        ext = formatExtension fmt
    canonicalFiles <- mapM canonicalizePath $ files cli
    let visibleSrcs = snd <$> filter ((`elem` canonicalFiles) . fst) results
        visibleAnchors = foldMap (getAnchors . fst) visibleSrcs
        syms = getSymbols $ fst <$> visibleSrcs
    moduleList <- forM visibleSrcs $ \src@(expSrc, _) -> do
      -- Generate Pandoc then write markdown
      (moduleIdent, pdoc) <- generatePandoc ext cli visibleAnchors syms src
      let docFilePath = joinPath moduleIdent
                          & flip addExtension ext
                          & combine (output cli)
      createDirectoryIfMissing True $ takeDirectory docFilePath
      UTF8.writeFile docFilePath $ writePandoc fmt pdoc

      -- Debug
      when (debug cli) $ do
        print docFilePath
        putDoc $ prettyExp expSrc
        pPrint pdoc

      return moduleIdent

    -- Write module index file
    sort moduleList
      & moduleForest ext
      & (header 1 "Modules" <>)
      & doc
      & writePandoc fmt
      & UTF8.writeFile (output cli </> "index" <.> ext)

writePandoc :: Format -> Pandoc -> String
writePandoc fmt pdoc = case lookup (formatPandoc fmt) writers of
  Just (TextWriter writer) -> case runPure $ writer opts pdoc of
      Right t -> T.unpack t
      Left perr -> error $ show perr
  _ -> error "Invalid writer."
  where
    opts :: WriterOptions
    opts = def{ writerExtensions = exts }
    exts :: Extensions
    exts = strictExtensions -- Necessary for raw html
             & enableExtension Ext_tex_math_dollars
             & enableExtension Ext_pipe_tables
             & enableExtension Ext_markdown_in_html_blocks
             & enableExtension Ext_backtick_code_blocks

formatPandoc :: Format -> Text
formatPandoc = \case
  Html     -> "html"
  Markdown -> "markdown"

formatExtension :: Format -> String
formatExtension = \case
  Html     -> "html"
  Markdown -> "md"

-- | Expand file patterns
withFilePatterns :: (Cli -> IO a) -> Cli -> IO a
withFilePatterns action cli = do
  files' <- flip mconcatMapM (files cli) $ \file ->
              if isRelative file
                then getDirectoryFiles "." [file]
                else let (drive, path) = splitDrive file
                     in map (joinDrive drive) <$> getDirectoryFiles drive [path]
  action cli{ files = files' }

-- | Exclude non-target device modules
withTargetDevice :: (Cli -> a) -> Cli -> a
withTargetDevice action cli = let files' = filter keepTargetDevice $ files cli
                              in action cli{ files = files' }
  where
    keepTargetDevice :: FilePath -> Bool
    keepTargetDevice file = flip all matches $ \case
      Just [tarDev, _, _] -> tarDev == target_device cli
      _ -> True
      where
        matches = devPats <&> (`match` file)
        devPats = import_dir cli <&> (</> "device" </> "*" </> "**" </> "*.k")
