{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.IO
    ( -- * Parsing
      module Language.Kanagawa.Parser.Options
    , parseProgram
    , parseFile
      -- * Errors
    , exitError
    , exitErrors
    ) where

import qualified Control.Exception as Ex
import Control.Monad (forM)
import Control.Monad.State (gets, liftIO, modify)
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Language.Kanagawa.Error (FileException(FileException))
import Language.Kanagawa.Internal (readFileUtf8)
import Language.Kanagawa.Parser
    ( Cache
    , ParseResult
    , SymbolTable
    , cmdargsSpecialModule
    , optionsSpecialModule
    , parseKanagawa
    )
import Language.Kanagawa.Parser.Lexer (Config(Config))
import Language.Kanagawa.Parser.Options
import Language.Kanagawa.Parser.SymbolTable
    (Needed(Needed), addImport, emptySymbolTable)
import System.Directory (canonicalizePath, findFile, getCurrentDirectory)
import System.Exit
import System.FilePath (FilePath, takeDirectory, (</>), (<.>))
import Text.Megaparsec.Error
import Text.Megaparsec.Stream

buildModule :: String -> [String] -> [(String, String)] -> [(String, String)] -> String
buildModule self modules consts aliases =
    "module " ++ self ++
    "{" ++ expose (self:modules) ++ "}" ++
    imports modules ++ "\n" ++
    defs "const auto " consts ++
    defs "using " aliases
  where
    defs x = intercalate "\n" . map (\(n, v) -> x ++ n ++ " = " ++ v ++ ";")
    imports = intercalate "\n" . map ("import " ++)
    expose = intercalate ",\n" . map ("module " ++)

buildCmdargsSpecialModule :: ParseOptions -> String
buildCmdargsSpecialModule opt = buildModule cmdargsSpecialModule [] (nameValue <$> define opt) (nameValue <$> using opt)
  where
    nameValue [] = ([], [])
    nameValue (x:xs')
       | x == '#' || x == '=' = ([], xs')
       | otherwise            = let (ys, zs) = nameValue xs' in (x:ys, zs)

buildOptionsSpecialModule :: ParseOptions -> String
buildOptionsSpecialModule ParseOptions{..} = buildModule optionsSpecialModule ["compiler.options"] options []
  where
    options = [ ("backend",             "Backend::" ++ show backend)
              , ("stall",               show stall)
              , ("optimize",            show optimize)
              , ("max_threads_default", show maxThreadsDefault)
              , ("max_threads_limit",   show maxThreadsLimit)
              ]

parseProgram :: ParseOptions -> Cache [ParseResult]
parseProgram opt = importBase >>= \case
    Left e -> return [Left e]
    Right symbolTable -> forM (files opt) (parseFile opt symbolTable)
  where
    importBase = do
        let baseFile
              | noImplicitBase opt || null (importDir opt) = cmdargsSpecialModule <.> "k"
              | otherwise                                  = baseLibrary opt
        b <- parseFile opt emptySymbolTable baseFile
        return $ addImport Needed 0 Nothing emptySymbolTable . snd <$> b

parseFile :: ParseOptions -> SymbolTable -> FilePath -> Cache ParseResult
parseFile opt symbolTable path = do
    cwd <- liftIO getCurrentDirectory
    r <- parseFile' [] cwd path
    either (Ex.throw . FileException) return r
  where
    parseFile' ancestorFiles dir file = do
        x <- liftIO $ readFileContent dir file
        either (return . Left) parseContent x
      where
        parseContent (canonicalPath, fileDir, content)
            | canonicalPath `elem` ancestorFiles = return $ Left "Circular dependency"
            | otherwise = gets (lookup canonicalPath) >>= \case
                Nothing -> do
                    let parseFileCallback = parseFile' (canonicalPath : ancestorFiles) fileDir
                        cfg = Config (parseDocs opt) False False
                    result <- parseKanagawa parseFileCallback cfg symbolTable canonicalPath content
                    modify (++ [(canonicalPath, result)])
                    return $ Right result
                Just result -> return $ Right result

    readFileContent dir file
        | file == cmdargsSpecialModule <.> "k" = return $ Right (file, dir, buildCmdargsSpecialModule opt)
        | file == optionsSpecialModule <.> "k" = return $ Right (file, dir, buildOptionsSpecialModule opt)
    readFileContent dir file = findFile (dir:importDirs) file >>= \case
        Just path' -> do
            canonicalPath <- canonicalizePath path'
            let displaySomeException = Ex.displayException :: Ex.SomeException -> String
            bimap displaySomeException (canonicalPath, takeDirectory path',) <$> Ex.try (readFileUtf8 path')
        Nothing -> return $ Left ("Can't find file: " ++ file ++ " in any of the import directories\n" ++ unlines importDirs)

    importDirs = map (</> "device" </> targetDevice opt) (importDir opt) ++ importDir opt

-- Print parse error(s) and exit with error code
exitErrors :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => [ParseErrorBundle s e] -> IO()
exitErrors e = exitError $ "Error 1: " ++ concatMap errorBundlePretty e

exitError :: String -> IO()
exitError x = do
  putStrLn x
  exitFailure
