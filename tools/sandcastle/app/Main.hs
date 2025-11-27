{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
{-# OPTIONS_GHC -fno-cse #-}

import Control.Monad
import Sandcastle
import System.Console.CmdArgs
import System.Environment
import System.Exit

main :: IO ()
main = do
    nullArgs <- null <$> getArgs
    cli' <- (if nullArgs then withArgs ["--help"] else id) $ cmdArgs cli
    when (null $ files cli') $ die "Missing source filename(s)"
    app cli'

cli :: Cli
cli = Cli
    { base_library     = "base.k" &= help "Base library file path" &= typFile
    , debug            = False &= help "Print debug messages"
    , define           = [] &= help "Define a global constant" &= name "define" &= name "d" &= explicit &= typ "name#val"
    , files            = [] &= typFile &= args
    , format           = Markdown &= help ("Documentation format. Supported formats are:" <> formats) &= typ "FORMAT"
    , import_dir       = [] &= help "Directories to search for imported files" &= typDir
    , no_implicit_base = False &= help "Disable implicit import of base library"
    , output           = "." &= help "Output directory for generated documentation, default is current working directory" &= typDir
    , source_url       = [] &= help "Root url for links to the source files" &= typ "URL"
    , svgbob           = False &= help "Generate Svgbob diagrams"
    , target_device    = "Arria-10-GX-1150" &= help "Target FPGA device name" &= typ "STRING"
    , using            = [] &= help "Define a global type alias" &= name "using" &= name "u" &= explicit &= typ "name#type"
    } &= summary copyright
      &= program "sandcastle"
      &= details ["Generate documentation."]

formats :: String
formats = ("\n  " <>) . show =<< [(minBound :: Format)..]

copyright :: String
copyright = unlines
  [ "(c) Microsoft Corporation"
  , "GNU GPL, version 2 or above"
  ]
