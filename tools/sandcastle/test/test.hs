{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
import Sandcastle
import System.FilePath
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

main :: IO ()
main = defaultMain =<< goldenTests

-- | File structure:
-- Current directory @.@ is @src/sandcastle@.
-- Test directory is @./test@.
-- Sample source code: @./test/sample/*.k@
-- Golden markdown: @./test/golden/sample/*.md@
-- Output (actual) markdown: @./test/output/sample/*.md@
goldenTests :: IO TestTree
goldenTests = do
  pdFiles <- findByExtension [".k"] $ "test" </> "sample"
  runApp pdFiles
  svgFiles <- findByExtension [".svg"] $ "test" </> "output"
  return $ testGroup  "Test"
    [ testGroup "Source to Markdown golden tests"
        [ goldenVsFile testName goldenFilePath outputFilePath (runApp pdFiles)
        | file <- "index" : map (joinPath . tail . splitPath) pdFiles
        , let testName = takeBaseName file
              goldenFilePath = "test" </> "golden" </> file -<.> "md"
              outputFilePath = "test" </> "output" </> file -<.> "md"
        ]
    , testGroup "Source to SVG golden tests"
        [ goldenVsFile testName goldenFilePath outputFilePath (runApp pdFiles)
        | file <- map (joinPath . drop 2 . splitPath) svgFiles
        , let testName = file
              goldenFilePath = "test" </> "golden" </> file -<.> "svg"
              outputFilePath = "test" </> "output" </> file -<.> "svg"
        ]
    ]
  where
    runApp :: [FilePath] -> IO ()
    runApp srcFiles =
        app Cli{ base_library     = ""
               , debug            = False
               , define           = []
               , files            = srcFiles
               , format           = Markdown
               , import_dir       = ["test"]
               , no_implicit_base = True
               , output           = "test" </> "output"
               , svgbob           = True
               , target_device    = ""
               , using            = []
               }
