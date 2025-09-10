{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Prettyprinter as PP
import Prettyprinter.Render.String
import GHC.Conc
import Language.Kanagawa.Error
import Language.Kanagawa.Frontend
import Language.Kanagawa.Internal (readFileUtf8)
import Language.Kanagawa.Parser.IO
import Language.Kanagawa.Parser.Syntax
import Language.Kanagawa.PrettyPrint
import Language.Kanagawa.Type
import Language.Kanagawa.Warning
import Options (Layout(Pretty, Smart), Options(Compile, PrettyPrint))
import Options.CmdArgs
import qualified Options as O
import ParseTree
import System.Directory
import System.Environment (getArgs, withArgs, getProgName)
import System.Exit
import System.IO
import Text.Megaparsec.Error

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    options <- (if null args then withArgs ["--help=all"] else id) getOptions
    if null $ O.files options
        then exitError "Missing source filename(s)"
        else handle options $ unwords $ prog : args

handle :: Options -> String -> IO ()

-- Pretty print files specified via command line options
handle opt@PrettyPrint{..} _ = do
    results <- evalStateT (parseProgram $ getParseOptions opt) []
    let parseErrors = lefts results
    if not $ null parseErrors
        then exitErrors parseErrors
        else forM_ (rights results) (sourcePretty . fst) >> exitSuccess
  where
    sourcePretty = putStrLn . renderString . engine layout . prettySource indent . removeRedundantNameScope
    layoutOptions = PP.LayoutOptions (PP.AvailablePerLine line_width ribbon)
    engine Pretty = PP.layoutPretty layoutOptions
    engine Smart = PP.layoutSmart layoutOptions

-- Parse files specified via command line options, merging the results into a
-- single AST, and then inject definitions specified via command line options
-- and rewrite the tree using frontend passes, and run compiler backend on
-- the final AST.
handle opt@Compile{..} cmdArgs = do
    numProc <- getNumProcessors
    let capabilities = case jobs of
         Just n
            | n > 0     -> n
            | otherwise -> numProc + n
         Nothing        -> numProc
    setNumCapabilities capabilities
    parsedFiles <- flip execStateT [] $ parseProgram $ getParseOptions opt
    let (fileNames, results) = unzip parsedFiles
        parseErrors = lefts results
        exprs = map fst $ rights results
    if not $ null parseErrors
        then exitErrors parseErrors
        else do
            let desugared = foldr1 append $ frontend passes template_passes template_iterations exprs
            when dump_parse $
                forM_ exprs $ print . prettyExp
            when dump_types $
                print $ prettyTypedExp desugared
            let desugarErrors = nub $ errors $ unnoteType desugared
            if not $ null desugarErrors
                then exitErrors =<< makeErrorBundles readFileUtf8 desugarErrors
                else codegen fileNames desugared
  where
    codegen fileNames program = do
        let untypedProgram = unnoteType program
        when dump_program $
            print $ prettyExp untypedProgram
        when dump_source $
            putStrLn $ renderString $ PP.layoutPretty PP.defaultLayoutOptions $ prettySource 4 program
        ws <- makeErrorBundles readFileUtf8 $ warnings (enabledWarnings warnings_state) untypedProgram
        mapM_ (putStrLn . errorBundlePretty) ws
        when (not (null ws) && warnings_as_errors) $
            exitError "Error 1: Warnings treated as errors"
        hFlush stdout
        success <- compile opt cmdArgs fileNames program
        when (not (null file_list) && not (null fileNames)) $
            updateFileList file_list $ filter (('.' /=) . head) $ sort fileNames
        if success
            then exitSuccess
            else exitFailure

    append (NotedExp _ (SeqF x)) (NotedExp n (SeqF y)) = NotedExp n (SeqF (x ++ y))
    append _ _ = undefined

-- Run languange server
--handle LangServer{..} = runLangServer log_file

-- Print usage
handle opt _ = print opt

updateFileList :: FilePath -> [FilePath] -> IO ()
updateFileList listfile parsedFiles = do
    old <- lines <$> listfileContent
    when (old /= parsedFiles) $
        writeFile listfile $ unlines parsedFiles
  where
    listfileContent = do
        exists <- doesFileExist listfile
        if exists
            then T.unpack <$> TIO.readFile listfile
            else return ""

getParseOptions :: Options -> ParseOptions
getParseOptions opt = case opt of
    Compile{..} -> defaultOptions
        { baseLibrary           = base_library
        , define                = define
        , files                 = files
        , importDir             = import_dir
        , noImplicitBase        = no_implicit_base
        , parseDocs             = parse_docs
        , stall                 = stall
        , optimize              = optimize
        , backend               = backend
        , targetDevice          = target_device
        , using                 = using
        , maxThreadsLimit       = max_threads_limit
        , maxThreadsDefault     = max_threads_default
        }
    PrettyPrint{..} -> defaultOptions
        { baseLibrary           = base_library
        , define                = define
        , files                 = files
        , importDir             = import_dir
        , noImplicitBase        = no_implicit_base
        , parseDocs             = parse_docs
        , targetDevice          = target_device
        , using                 = using
        }
    _ -> error "Unsupported mode."
