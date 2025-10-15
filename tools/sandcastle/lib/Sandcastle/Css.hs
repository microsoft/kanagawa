{-# LANGUAGE OverloadedStrings #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle.Css
  ( renderHtmlInline
  , htmlStyle
  , signatureStyle
  , anchorStyle
  , attributeStyle
  , builtinStyle
  , identifierStyle
  , keywordStyle
  , linkStyle
  , literalStyle
  , modifierStyle
  ) where

import Clay hiding (red, orange, yellow, green, blue, purple, gray)
import qualified Clay as C
import Clay.Render
import Data.String
import Data.Text.Lazy (Text)

renderHtmlInline :: Css -> Text
renderHtmlInline = renderWith htmlInline []

htmlStyle :: Css
htmlStyle = mconcat
  [ aStyle
  , containerStyle
  , navStyle
  , contentStyle
  , signatureStyle
  , bodyStyle
  , docsStyle
  , docsDetailsStyle
  , headersStyle
  , kanagawaStyle
  , syntaxStyle
  , noBulletStyle
  , codeStyle
  , navListStyleType
  , navListPadding
  ]

aStyle :: Css
aStyle = do
  a         ? textDecoration none
  a # hover ? textDecoration underline

containerStyle :: Css
containerStyle = ".container" ? do
  display       flex
  flexDirection row

navStyle :: Css
navStyle = ".nav" ? do
  position          sticky
  flexBasis         (pct 10)
  backgroundColor   antiFlashWhite
  padding           (px 20) (px 20) (px 20) (px 20)
  top               (vh 10)
  height            maxContent
  maxHeight         (vh 80)
  overflowY         auto
  minWidth          maxContent
  margin            (em 3) (em 3) (em 3) (em 3)

navListPadding :: Css
navListPadding = ".nav" |> "ul" ? paddingLeft (px 0)

navListStyleType :: Css
navListStyleType = ".nav" C.** "ul" ? listStyleType none

contentStyle :: Css
contentStyle = ".content" ? do
  flexBasis         (pct 80)
  maxWidth          (px 1450)
  margin            (em 3) auto (em 3) auto

signatureStyle :: Css
signatureStyle = ".signature" ? do
  fontFamily        [] [monospace]
  backgroundColor   antiFlashWhite
  color             "#373d3f"
  border            (px 1) solid "#ddd"
  borderLeft        (px 3) solid "#0080FF"
  borderRadius      (px 5) (px 5) (px 5) (px 5)
  marginBottom      (em (1/2))
  padding           (em (1/2)) (em 1) (em (1/2)) (em 1)

bodyStyle :: Css
bodyStyle = ".body" ? do
  fontFamily        [""] [sansSerif]
  fontSize          (em 1)
  padding           (px 10) (px 10) (px 10) (px 10)
  color             hokusaiDarkGray
  lineHeight        (unitless 1.4)

docsStyle :: Css
docsStyle = ".docs" ? do
  paddingLeft       (em 1)
  marginTop         (em (-0.4))
  marginBottom      (em 1.4)
  borderLeft        (px 1) solid "#DCDCDC"

docsDetailsStyle :: Css
docsDetailsStyle = ".docs details" ? do
  marginTop         (em 1)

headersStyle :: Css
headersStyle = ".body h1, h2, h3, h4" ? do
  color             hokusaiDarkBlue

kanagawaStyle :: Css
kanagawaStyle = ".kanagawa" ? do
  backgroundColor   "#f7f7f7"
  padding           (em 1) (em 1) (em 1) (em 1)

syntaxStyle :: Css
syntaxStyle = mconcat
  [ anchorStyle
  , attributeStyle
  , builtinStyle
  , identifierStyle
  , keywordStyle
  , linkStyle
  , literalStyle
  , modifierStyle
  ]

anchorStyle :: Css
anchorStyle = ".anchor" ? color gray

attributeStyle :: Css
attributeStyle = ".attribute" ? color orange

builtinStyle :: Css
builtinStyle = ".builtin" ? color yellow

identifierStyle :: Css
identifierStyle = ".identifier" ? fontWeight bold

keywordStyle :: Css
keywordStyle = ".keyword" ? color green

linkStyle :: Css
linkStyle = ".link" ? color blue

literalStyle :: Css
literalStyle = ".literal" ? color red

modifierStyle :: Css
modifierStyle = ".modifier" ? color purple

noBulletStyle :: Css
noBulletStyle = ".subList" ? listStyleType none

codeStyle :: Css
codeStyle = "code" ? do
  fontFamily [] [monospace]
  fontSize $ em (6/5)

red :: IsString c => c
red = "#D73737"

orange :: IsString c => c
orange = "#ffa500"

yellow :: IsString c => c
yellow = "#CFB017"

green :: IsString c => c
green = "#60AC39"

blue :: IsString c => c
blue = "#6684E1"

purple :: IsString c => c
purple = "#B854D4"

gray :: IsString c => c
gray = "#C5C8C6"

hokusaiDarkBlue :: IsString c => c
hokusaiDarkBlue = "#283665"

hokusaiDarkGray :: IsString c => c
hokusaiDarkGray = "#1c1b24"

antiFlashWhite :: IsString c => c
antiFlashWhite = "#f2f3f4"
