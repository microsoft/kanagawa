{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle.Tree
  ( drawPowerline
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree

-- | 'Data.Tree.drawForest'
drawPowerline :: [Tree Text] -> Text
drawPowerline = T.unlines . concatMap draw

draw :: Tree Text -> [Text]
draw (Node x trees) = T.lines x ++ drawSubTrees trees
  where
    drawSubTrees :: [Tree Text] -> [Text]
    drawSubTrees = \case
      []   -> []
      [t]  -> shift rAngle sp (draw t)
      t:ts -> shift rTee vlPipe (draw t) ++ drawSubTrees ts
      where
        shift :: Text -> Text -> [Text] -> [Text]
        shift first other = zipWith (<>) (first : repeat other)
        rTee   = "\x251C\x2500\x00A0" -- "├─ "
        vlPipe = "\x2502\x00A0\x00A0" -- "│  "
        rAngle = "\x2514\x2500\x00A0" -- "└─ "
        sp     = "\x00A0\x00A0\x00A0" -- "   "
