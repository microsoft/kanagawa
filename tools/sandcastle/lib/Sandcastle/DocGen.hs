{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Copyright : (c) Microsoft Corporation
License   : GNU GPL, version 2 or above
-}
module Sandcastle.DocGen
    ( generatePandoc
    , moduleForest
    , getAnchors
    ) where

import qualified Clay as C
import Control.Exception
import Control.Monad.Extra
import Control.Monad.RWS
import Data.Char (isSpace)
import Data.Either
import Data.Function
import Data.Functor
import qualified Data.HashMap.Lazy as HashMap
import Data.List.Extra hiding (group, list)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Tree
import Prettyprinter hiding (sep, space)
import qualified Prettyprinter as P
import Prettyprinter.Render.Util.SimpleDocTree
import Language.Kanagawa.Desugar
import Language.Kanagawa.Parser
import Language.Kanagawa.Parser.SymbolTable hiding (symbols)
import Language.Kanagawa.Parser.Syntax hiding (Div, body)
import Language.Kanagawa.Parser.Syntax.FixPattern
import Language.Kanagawa.PrettyPrint ()
import Language.Kanagawa.Recursion
import Language.Kanagawa.Symbols
import Sandcastle.Cli (Cli, Format(..))
import qualified Sandcastle.Cli as Cli
import Sandcastle.Css
import Sandcastle.Monad
import Sandcastle.Tree
import System.Directory
import System.FilePath
import qualified System.FilePath.Posix as Posix
import System.Process
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text
import Text.Pandoc
import Text.Pandoc.Builder hiding (code, note)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Walk
import Text.Megaparsec.Pos

-- | Read 'ExpSrc' and generate 'Pandoc' data.
generatePandoc
  :: String                -- ^ File format extension
  -> Cli
  -> Set QualifiedName     -- ^ anchors
  -> Symbols
  -> (ExpSrc, SymbolTable)
  -> IO ([String], Pandoc)
generatePandoc ext cli ancs syms e = runApp ext cli ancs syms (pModule e)
                                       <&> fmap doc
                                       <&> fmap (walk foldExamples)
                                       <&> fmap (walk styleCodeBlocks)
                                       <&> fmap (walk renderSubLists)
  where
    renderSubLists = case Cli.format cli of
      Html     -> subListToHtml
      Markdown -> subListToMd

styleCodeBlocks :: Block -> Block
styleCodeBlocks = \case
    CodeBlock (i, [], kv) t -> CodeBlock (i, ["kanagawa"], kv) t
    b -> b

-- | Wrap h4 titled "Example(s)" in a details element.
-- Walk the document, wrap example headers in a detail and
-- span the following code blocks or paragraph-codeblock pairs.
foldExamples :: [Block] -> [Block]
foldExamples = go
  where
    go = \case
      [] -> []
      Header 4 _ [Strong [Str h]] : bs
        | h == "Example" || h == "Examples" ->
          let (es, bs') = spanExampleBlocks bs
          in mconcat
               [ [ RawBlock "html" "<details>"
                 , RawBlock "html" $ "<summary>" <> h <> "</summary>"
                 ]
               , unpackExample =<< es
               , [RawBlock "html" "</details>"]
               , go bs'
               ]
      b : bs -> b : go bs
      where
        unpackExample :: (Maybe Block, Block) -> [Block]
        unpackExample = \case
          (Just p,  c) -> [p, c]
          (Nothing, c) -> [c]

-- | Get next code blocks, with optional preceding paragraph.
spanExampleBlocks
  :: [Block]
  -> ([(Maybe Block, Block)], [Block])
spanExampleBlocks = \case
  p@Para{} : c@CodeBlock{} : bs ->
    let (es, bs') = spanExampleBlocks bs
    in ((Just p, c) : es, bs')
  c@CodeBlock{} : bs ->
    let (es, bs') = spanExampleBlocks bs
    in ((Nothing, c) : es, bs')
  bs -> ([], bs)

----------------
-- Renderings --
----------------

-- | Signature annotations
data SigAnn = Anchor   Text -- ^ href
                       Text -- ^ id
            | Source   Text -- ^ href
            | Att
            | Builtin
            | Ident
            | Keyword
            | Linked   Text -- ^ href
            | Literal
            | Modifier

data Rendering = Rendering
  { sig    :: Doc SigAnn -- ^ signature
  , sanSig :: Blocks     -- ^ blocks without signature
  , conSig :: Blocks     -- ^ blocks with signature
  }

instance IsString (App Rendering) where
  fromString = mkSig . fromString

-- | 'Rendering' smart constructor. Append 'sanSig' to a pretty
-- printed signature to construct 'conSig'.
mkRendering :: (Doc SigAnn, Blocks) -> App Rendering
mkRendering (s, ss) = do
  s' <- renderSig s
  ss' <- renderDocs ss
  return $ Rendering
    { sig    = s
    , sanSig = ss
    , conSig = s' <> ss'
    }

renderSig :: Doc SigAnn -> App Blocks
renderSig = pre . TL.toStrict <=< render . layoutSmart defaultLayoutOptions

renderDocs :: Blocks -> App Blocks
renderDocs x = if (x == mempty)
  then return x
  else do
    asks format <&> \case
      Markdown -> x
      Html -> mconcat
        [ blockHtml "<div class=\"docs\">"
        , x
        , blockHtml "</div>"
        ]

mkSig :: Doc SigAnn -> App Rendering
mkSig s = mkRendering (s, mempty)

mapSig :: (Doc SigAnn -> Doc SigAnn) -> App Rendering -> App Rendering
mapSig f r = do
  r' <- r
  let sig' = f $ sig r'
  sig'' <- renderSig sig'
  let conSig' = sig'' <> sanSig r'
  return r'{ sig = sig', conSig = conSig' }

render :: SimpleDocStream SigAnn -> App TL.Text
render sds = asks format <&> \fmt ->
  TLB.toLazyText $ renderHtmlBuilder $ renderTree fmt (treeForm sds)

renderTree :: Cli.Format -> SimpleDocTree SigAnn -> Html
renderTree fmt = \case
  STEmpty           -> mempty
  STChar c          -> H.toHtml c
  STText _ t        -> H.toHtml t
  STLine i          -> H.toHtml $ "\n" <> textSpaces i
  STAnn ann content -> case fmt of
    Html     -> encloseInTagFor   ann $ renderTree fmt content
    Markdown -> encloseInMdTagFor ann $ renderTree fmt content
  STConcat contents -> foldMap (renderTree fmt) contents
  where
    textSpaces :: Int -> Text
    textSpaces n = T.replicate n (T.singleton ' ')

encloseInTagFor :: SigAnn -> Html -> Html
encloseInTagFor = \case
  Anchor href ident -> H.a    H.! HA.class_ "anchor"
                              H.! HA.href   (H.textValue href)
                              H.! HA.id     (H.textValue ident)
  Source href       -> H.a    H.! HA.class_ "source_link"
                              H.! HA.href   (H.textValue href)
  Att               -> H.span H.! HA.class_ "attribute"
  Builtin           -> H.span H.! HA.class_ "builtin"
  Ident             -> H.span H.! HA.class_ "identifier"
  Keyword           -> H.span H.! HA.class_ "keyword"
  Linked href       -> H.a    H.! HA.class_ "link"
                              H.! HA.href   (H.textValue href)
  Literal           -> H.span H.! HA.class_ "literal"
  Modifier          -> H.span H.! HA.class_ "modifier"

encloseInMdTagFor :: SigAnn -> Html -> Html
encloseInMdTagFor = \case
  Anchor href ident -> H.a    H.! HA.style (inline anchorStyle)
                              H.! HA.href  (H.textValue href)
                              H.! HA.id    (H.textValue ident)
  Source href       -> H.a    H.! HA.style (inline sourceLinkStyle)
                              H.! HA.href  (H.textValue href)
  Att               -> H.span H.! HA.style (inline attributeStyle)
  Builtin           -> H.span H.! HA.style (inline builtinStyle)
  Ident             -> H.span H.! HA.style (inline identifierStyle)
  Keyword           -> H.span H.! HA.style (inline keywordStyle)
  Linked href       -> H.a    H.! HA.style (inline linkStyle)
                              H.! HA.href  (H.textValue href)
  Literal           -> H.span H.! HA.style (inline literalStyle)
  Modifier          -> H.span H.! HA.style (inline modifierStyle)
  where
    inline = H.lazyTextValue . renderHtmlInline

instance Semigroup Rendering where
  x <> y = Rendering{ sig    = sig    x <> sig    y
                    , sanSig = sanSig x <> sanSig y
                    , conSig = conSig x <> conSig y
                    }

instance Monoid Rendering where
  mempty = Rendering
    { sig    = mempty
    , sanSig = mempty
    , conSig = mempty
    }

rAlias
  :: Rendering  -- ^ body
  -> Rendering  -- ^ ident
  -> Doc SigAnn -- ^ anchor
  -> Blocks     -- ^ description
  -> (Doc SigAnn, Blocks)
rAlias bdy ident anc desc =
  ( hang 4 $ P.sep
      [ annotate Keyword "using" <+> sig ident <+> "="
      , align (sig bdy) <+> anc
      ]
  , desc <> sanSig bdy
  )

rArray
  :: [Rendering] -- ^ attributes
  -> Rendering   -- ^ type
  -> [Rendering] -- ^ dimensions
  -> Doc SigAnn
rArray attrs ty dims = attrList attrs <> sig ty <> arrayDimList (sig <$> dims)

-- | TODO: Use L.P.PrettyPrinter
arrayDimList :: [Doc ann] -> Doc ann
arrayDimList = encloseSep lbracket rbracket (rbracket <> lbracket)

rBinary :: Doc SigAnn -> Rendering -> Rendering -> Doc SigAnn
rBinary bop a b = parens $ sig a <+> bop <+> sig b

rCast :: Rendering -> Rendering -> Doc SigAnn
rCast ty a = annotate Builtin "cast" <> (angles . sig) ty <> (parens . sig) a

data MemberGroup = Aliases
                 | Types
                 | Fields
                 | Functions
                 | Invariants
                 | General
  deriving (Eq, Ord, Read, Show)

memberGroup :: ExpSrc -> MemberGroup
memberGroup = \case
  Alias{}         -> Aliases
  Class{}         -> Types
  Enum{}          -> Types
  Function{}      -> Functions
  FunctionDecl{}  -> Functions
  StaticAssert{}  -> Invariants
  Struct{}        -> Types
  Template _ decl -> memberGroup decl
  Union{}         -> Types
  Variable{}      -> Fields
  _               -> General

rClass
  :: Rendering                  -- ^ identifier
  -> [(MemberGroup, Rendering)] -- ^ members
  -> Doc SigAnn                 -- ^ anchor
  -> Blocks                     -- ^ description
  -> (Doc SigAnn, Blocks)
rClass ident ms anc desc =
  ( annotate Keyword "class" <+> sig ident <+> anc
  , desc <> groupClassMembers ms
  )
  where
    groupClassMembers :: [(MemberGroup, Rendering)] -> Blocks
    groupClassMembers = mconcat . map mkGroup . groupSort
      where
        mkGroup :: (MemberGroup, [Rendering]) -> Blocks
        mkGroup (g, rs) = clist (groupName g) rs

        groupName Fields    = str "Callbacks and Fields"
        groupName Functions = str "Methods"
        groupName g         = str $ T.pack $ show g

rDesignator :: Rendering -> Rendering -> Doc SigAnn
rDesignator ident t = "." <> sig ident <+> "=" <+> sig t

rEnum
  :: Rendering   -- ^ ident
  -> Rendering   -- ^ type
  -> [Rendering] -- ^ values
  -> Doc SigAnn  -- ^ anchor
  -> Blocks      -- ^ docs
  -> (Doc SigAnn, Blocks)
rEnum ident et bdy anc desc =
  ( annotate Keyword "enum" <+> sig ident <+> ":" <+> sig et <+> anc
  , desc <> clist "Values" bdy
  )

-- | Render 'EnumConstant'
rEnumConstant
  :: Rendering       -- ^ ident
  -> Maybe Rendering -- ^ constant value
  -> Doc SigAnn      -- ^ anchor
  -> Blocks          -- ^ description
  -> (Doc SigAnn, Blocks)
rEnumConstant ident cvM anc desc =
  ( sig ident <> maybe mempty ((" =" <+>) . sig) cvM <+> anc
  , desc
  )

-- | Render 'FuncParam'
rFuncParam
  :: [Rendering] -- ^ attributes
  -> Rendering   -- ^ type
  -> Rendering   -- ^ ident
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rFuncParam attrs pt ident desc =
  ( attrList attrs <> sig pt <+> sig ident
  , desc <> sanSig pt
  )

rFunction
  :: [Rendering]     -- ^ attributes
  -> Maybe Rendering -- ^ modifier
  -> Rendering       -- ^ return type
  -> Rendering       -- ^ identifier
  -> [Rendering]     -- ^ arguments
  -> Doc SigAnn      -- ^ anchor
  -> Blocks          -- ^ description
  -> (Doc SigAnn, Blocks)
rFunction attrs fm rt ident fps anc desc =
  ( cat $ withAttrList attrs $ withModifier
      [ cat $
          sig rt <> flatAlt "" " " :
            [ nest 4 $ cat $ (<> [")" <+> anc]) $
                sig ident <> "(" : punctuate (flatAlt "," ", ") (sig <$> fps)
            ]
      ]
  , desc <> cflist "Arguments" fps
  )
  where
    withModifier :: [Doc SigAnn] -> [Doc SigAnn]
    withModifier = maybe id (\m -> (sig m :)) fm

rFunctionDecl
  :: [Rendering] -- ^ attributes
  -> Rendering   -- ^ return type
  -> Rendering   -- ^ identifier
  -> [Rendering] -- ^ arguments
  -> Doc SigAnn  -- ^ anchor
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rFunctionDecl attrs rt ident fps anc desc =
  ( cat $ withAttrList attrs
      [ cat $ sig rt <> flatAlt "" " " :
          [ nest 4 $ cat $ (<> [")" <+> anc]) $
              sig ident <> "(" : punctuate (flatAlt "," ", ") (sig <$> fps)
          ]
      ]
  , desc <> cflist "Arguments" fps
  )

rFunctionType
  :: [Rendering] -- ^ attributes
  -> [Rendering] -- ^ arguments
  -> Rendering   -- ^ return type
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rFunctionType attrs args rty desc =
  ( cat $ withAttrList attrs
      [ cat
          [ align $ hcat
              [ flatAlt "( " "("
              , cat $ punctuate (flatAlt "," ", ") $ sig <$> args
              ]
          , ") ->" <+> sig rty
          ]
      ]
  , desc <> cflist "Arguments" args
  )

rFunctionTypeParam
  :: [Rendering]     -- ^ attributes
  -> Rendering       -- ^ type
  -> Maybe Rendering -- ^ name
  -> Blocks          -- ^ description
  -> (Doc SigAnn, Blocks)
rFunctionTypeParam attrs ty nm desc =
  ( attrList attrs <> sig ty <> maybe mempty ((" " <>) . sig) nm
  , desc
  )

pModule :: (ExpSrc, SymbolTable) -> App ([String], Blocks)
pModule (expSrc, symTab) = case expSrc of
    Seq [_, ModuleDecl e@(NotedExp _ (ModuleIdentifierF qn)) _ (Seq bdy)] ->
        local (setModuleIdentifier qn) $ do
            -- Module header
            ext <- asks fileExt
            indexUrl <- rootPath <&> (: ["index" <.> ext])
                                 <&> Posix.joinPath
                                 <&> T.pack
            let moduleName = T.pack $ intercalate "." qn
                indexLink = link indexUrl "" $ inlineHtml "&equiv;"
                moduleHeader = header 1 $ mconcat
                    [text moduleName,  space,  indexLink]
                visible = visibleSymbols $ module_ symTab
                expos = HashMap.elems $ fromJust $ Map.lookup (namespace $ module_ symTab) visible
            -- Module index (needs work: sections)
            ixList <- indexList visible
            -- Module declarations
            let exposedNames = mapMaybe getIdent expos
                (exposedDecls, unexposedDecls) =
                  partition (maybe False (`elem` exposedNames) . getIdent) bdy

                hideUnexposedAnchors :: Env -> Env
                hideUnexposedAnchors r =
                  let unexposedQualifiedNames = S.fromList $ mapMaybe getQualifiedIdent unexposedDecls
                      anchors' = anchors r `S.difference` unexposedQualifiedNames
                  in r{anchors = anchors'}

                renderDecl = fmap conSig . term

            renderedDecls <- local hideUnexposedAnchors $ foldMap renderDecl exposedDecls
            ds <- describeDocs e
            bs <- formatModule (moduleHeader <> ds) ixList renderedDecls <$> asks format
            return (qn, bs)
    _ -> error "Invalid module"
  where
    indexList :: Map Name Symbols -> App Blocks
    indexList visible = do
        modList <- forM (Map.toList visible) $ \(modNamespace, modSymbols) -> do
            let modName = toString $ unmangleModuleNamespace modNamespace
            modPath <- T.pack <$> modulePath (toString modNamespace)
            let modLink = indexLink modPath $ text $ T.pack modName
                toplevelSymbols = HashMap.filterWithKey (\k _ -> length k == 2) modSymbols
            symbolList <- forM (sortOn fst $ HashMap.toList toplevelSymbols) $ \(qn, e) -> do
                symPath <- T.pack <$> linkPath (map fromName qn)
                let symName = fromJust $ getIdent e
                return $ indexLink symPath $ B.code symName
            return $ modLink <> bulletList symbolList
        return $ header 2 "Index" <> bulletList modList
      where
        indexLink url = plain . linkWith ("index-link", ["link"], []) url ""

formatModule
  :: Blocks -- ^ module header and description
  -> Blocks -- ^ module index list
  -> Blocks -- ^ rendered declarations
  -> Cli.Format
  -> Blocks
formatModule hdr ix rs = \case
  Html     -> styleBlocks htmlStyle <> body (container (nav <> content))
  Markdown -> hdr <> ix <> horizontalRule <> rs
  where
    container = divWith ("container", ["container"], [])
    nav       = divWith ("nav", ["nav"], []) ix
    content   = divWith ("content", ["content"], []) $ hdr <> horizontalRule <> rs
    body      = divWith ("body", ["body"], [])

styleBlocks :: C.Css -> Blocks
styleBlocks = blockHtml . TL.toStrict . renderHtml . H.style . H.toHtml . C.render

rMux :: Rendering -> Rendering -> Rendering -> Doc SigAnn
rMux a c b = sig a <+> align (cat [ "?" <+> sig c
                                  , flatAlt ":" " :" <+> sig b
                                  ])

displayLink
  :: (ExpSrc -> App Rendering)
  -> ExpSrc
  -> App (Maybe ExpSrc, Doc SigAnn)
displayLink go = \case
  e@(ScopedName s (Just a) b) -> do
    (resolvedPrefix, rndr) <- displayLink go a
    if "@" `isPrefixOf` getName a
      then linkSegment (getQualifiedName e) s resolvedPrefix b
      else fmap ((rndr <> "::") <>) <$> linkSegment (getQualifiedName e) s resolvedPrefix b
  e@(ScopedName s Nothing  b) -> do
    fmap (memptyWhenModule b) <$> linkSegment (getQualifiedName e) s Nothing b
  TemplateInstance a (Seq bs) -> do
    (resolvedPrefix, a') <- displayLink go a
    bs' <- mapM go bs
    a'' <- mkSig a'
    return (resolvedPrefix, rTemplateInstance a'' bs')
  _ -> error "Invalid display link"
  where
    memptyWhenModule b l
      | "@" `isPrefixOf` getName b = mempty
      | otherwise                  = l

linkSegment
  :: QualifiedName
  -> QualifiedName
  -> Maybe (NotedExp n e)
  -> NotedExp n e
  -> App (Maybe ExpSrc, Doc SigAnn)
linkSegment qn scope prefix segment = do
  let qualifiedSegment = copyNote (ScopedNameF scope prefix segment) segment
  symbol <- lookupSymbol qualifiedSegment <$> asks symbols
  let resolvedPrefix = flip (maybe symbol) symbol $ \case
        Alias _ (TypeIdentifier _ t) _ -> Just t
        _                              -> symbol
  url <- fmap T.pack $ linkPath $ toString <$> maybe qn getQualifiedName symbol
  hasAnchor <- asks $ case symbol of
    Just e  -> S.member (getQualifiedName e) . anchors
    Nothing -> S.member qn . anchors
  let annLinkIfAnchor = if hasAnchor
                          then annotate (Linked url)
                          else id
  return (resolvedPrefix, annLinkIfAnchor $ annotate Ident $ pretty segment)

rStruct
  :: Rendering   -- ^ identifier
  -> [Rendering] -- ^ body
  -> Doc SigAnn  -- ^ anchor
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rStruct ident bdy anc desc =
  ( annotate Keyword "struct" <+> sig ident <+> anc
  , desc <> clist "Fields" bdy
  )

rTemplate
  :: [Rendering] -- ^ parameters
  -> Rendering   -- ^ type
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rTemplate ps t desc =
  ( vsep [tParams, sig t]
  , desc <> cflist "Parameters" ps <> sanSig t
  )
  where
    tParams :: Doc SigAnn
    tParams = nest 4 $ cat $ (<> [">"]) $
      annotate Keyword "template" <+> "<" : punctuate (flatAlt "," ", ") (sig <$> ps)

rTemplateInstance :: Rendering -> [Rendering] -> Doc SigAnn
rTemplateInstance a bs = hang 4 $ cat
  [ sig a <> "<"
  , align $ cat (punctuate (flatAlt "," ", ") (sig <$> bs) <> [">"])
  ]

rTemplateParam
  :: Rendering       -- ^ type
  -> Name
  -> Maybe Rendering -- ^ default
  -> Blocks          -- ^ description
  -> (Doc SigAnn, Blocks)
rTemplateParam ty nm dtM desc =
  ( case dtM of
      Just dt -> nest 4 $ P.sep [tyNm, "=" <+> sig dt]
      Nothing -> tyNm
  , sanSig ty <> desc
  )
  where
    tyNm :: Doc SigAnn
    tyNm = sig ty <+> (fromString . fromName) nm

rUnion
  :: Rendering   -- ^ identifier
  -> [Rendering] -- ^ body
  -> Doc SigAnn  -- ^ anchor
  -> Blocks      -- ^ description
  -> (Doc SigAnn, Blocks)
rUnion ident bdy anc desc =
  ( annotate Keyword "union" <+> sig ident <+> anc
  , desc <> clist "Fields" bdy
  )

rVariable
  :: [Rendering]       -- ^ attributes
  -> Rendering         -- ^ type
  -> Rendering         -- ^ identifier
  -> Maybe Rendering -- ^ value
  -> Doc SigAnn      -- ^ anchor
  -> Blocks            -- ^ description
  -> (Doc SigAnn, Blocks)
rVariable attrs vt ident viM anc desc =
  ( attrList attrs <> sig vt <+> sig ident <> maybe mempty ((" =" <+>) . sig) viM <+> anc
  , desc
  )

------------
-- Terms --
------------

term :: ExpSrc -> App Rendering
term e = case e of
  Alias _ bdy ident -> mkRendering =<<
    rAlias <$> term bdy
           <*> term ident
           <*> anchor e
           <*> describeDocs e
  Array (Seq attrs) ty (Seq dims) -> mkSig =<<
    rArray <$> mapM term attrs
           <*> term ty
           <*> mapM term dims
  Attr attr a    -> mapSig ((annotate Att (pretty attr) <>) . parens) $ term a
  Auto           -> mkSig $ annotate Keyword $ pretty e
  Binary bop a b -> mkSig =<< rBinary (pretty bop) <$> term a <*> term b
  BoolLiteral{}  -> mkSig $ annotate Literal $ pretty e
  Boolean        -> mkSig $ annotate Builtin $ pretty e
  Cast ty a      -> mkSig =<< rCast <$> term ty <*> term a
  Class _ ident (Seq bdy) -> do
    let labelClassMembers = map (\m -> (memberGroup m, m)) (classMembers bdy)
    ms <- (mapM . mapM) term labelClassMembers
    let ms' = filter ((mempty /=) . conSig . snd) ms
    mkRendering =<<
      rClass <$> term ident
             <*> return ms'
             <*> anchor e
             <*> describeDocs e
  Const a      -> mapSig (annotate Keyword "const" <+>) $ term a
  Decltype _ a -> mapSig ((annotate Builtin "decltype" <>) . parens) $ term a
  Designator ident t -> mkSig =<< rDesignator <$> term ident <*> term t
  Enum _ ident et (Seq bdy) -> mkRendering =<<
    rEnum <$> term ident
          <*> term et
          <*> mapM term bdy
          <*> anchor e
          <*> describeDocs e
  EnumConstant _ ident cvM -> mkRendering =<<
    rEnumConstant <$> term ident
                  <*> mapM term cvM
                  <*> anchor e
                  <*> describeDocs e
  EnumValue v _ -> term v
  Extern (Seq attrs) b -> do
    attrs' <- mapM term attrs
    b'     <- term b
    mkSig $ attrList attrs' <> annotate Keyword "extern" <+> sig b'
  FlagAttr{}     -> mkSig $ annotate Att     $ pretty e
  Float          -> mkSig $ annotate Builtin $ pretty e
  FloatLiteral{} -> mkSig $ annotate Literal $ pretty e
  FuncParam _ (Seq attrs) pt ident -> mkRendering =<<
    rFuncParam <$> mapM term attrs
               <*> term pt
               <*> term ident
               <*> describeDocs e
  Function _ _ (Seq attrs) fm rt ident (Seq fps) _ -> mkRendering =<<
    rFunction <$> mapM term attrs
              <*> mapM term fm
              <*> term rt
              <*> term ident
              <*> mapM term fps
              <*> anchor e
              <*> describeDocs e
  FunctionDecl _ (Seq attrs) rt ident (Seq fps) -> mkRendering =<<
    rFunctionDecl <$> mapM term attrs
                  <*> term rt
                  <*> term ident
                  <*> mapM term fps
                  <*> anchor e
                  <*> describeDocs e
  FunctionModifier{} -> mkSig $ annotate Modifier $ pretty e
  FunctionType (Seq attrs) (Seq args) rTy -> mkRendering =<<
    rFunctionType <$> mapM term attrs
                  <*> mapM term args
                  <*> term rTy
                  <*> describeDocs e
  FunctionTypeParam (Seq attrs) ty nm -> mkRendering =<<
    rFunctionTypeParam <$> mapM term attrs
                       <*> term ty
                       <*> mapM term nm
                       <*> describeDocs e
  Identifier n             -> mkSig $ annotate Ident $ pretty n
  InitializerList (Seq l)  -> mkSig . align . encloseSep lbrace rbrace comma . map sig =<< mapM term l
  IntAttr attr i           -> mapSig ((annotate Att (pretty attr) <>) . parens) $ term i
  IntLiteral{}             -> mkSig $ annotate Literal $ pretty e
  Integer{}                -> mkSig $ pretty e
  MemberAccess a b         -> term a <> "." <> term b
  Mux a (Seq [b, c])       -> mkSig =<< rMux <$> term a <*> term c <*> term b
  NamedValue n             -> term n
  ParamInt n               -> mapSig ((annotate Builtin "int"  <>) . angles) $ term n
  ParamUint n              -> mapSig ((annotate Builtin "uint" <>) . angles) $ term n
  Positional _ t           -> term t
  QualifiedIdentifier a    -> mkSig . snd =<< displayLink term a
  Reference a              -> mkSig . (<> "&") . sig =<< term a
  StaticAssert a           -> mkRendering =<< (,) <$> sig `fmap` term a
                                                  <*> describeDocs e
  String                   -> mkSig $ annotate Builtin $ pretty e
  StringLiteral{}          -> mkSig $ annotate Literal $ pretty e
  InterpolatedString{}     -> mkSig $ annotate Literal $ pretty e
  Struct _ ident (Seq bdy) -> mkRendering =<<
    rStruct <$> term ident
            <*> mapM term bdy
            <*> anchor e
            <*> describeDocs e
  Template tps tt -> mkRendering =<<
    rTemplate <$> mapM term tps
              <*> term tt
              <*> describeDocs e
  TemplateInstance a (Seq bs) -> mkSig =<< rTemplateInstance <$> term a <*> mapM term bs
  TemplateParam _ ty _ _ dtM -> mkRendering =<<
    rTemplateParam <$> term ty
                   <*> (pure . getName) e
                   <*> mapM term dtM
                   <*> describeDocs e
  TypeIdentifier _ a -> mkSig . snd =<< displayLink term a
  TypeParam{}        -> mkSig $ pretty e
  Typename           -> mkSig $ annotate Keyword $ pretty e
  Unary uop a        -> mkSig . (annotate Builtin (pretty uop) <>) . parens . sig =<< term a
  Union _ ident (Seq bdy) -> mkRendering =<<
    rUnion <$> term ident
           <*> mapM term bdy
           <*> anchor e
           <*> describeDocs e
  Unsigned{} -> mkSig $ annotate Keyword $ pretty e
  Variable _ (Seq attrs) vt ident viM _ -> mkRendering =<<
    rVariable <$> mapM term attrs
              <*> term vt
              <*> term ident
              <*> mapM term viM
              <*> anchor e
              <*> describeDocs e
  Void -> mkSig $ annotate Keyword $ pretty e
  _    -> foldMap term $ unfix e

classMembers
  :: [ExpSrc]   -- ^ class body
  -> [ExpSrc]
classMembers bdy = catMaybes $ flip mapClassMembers bdy $ curry $ \case
  (PublicMember,  m) -> case m of
    Public  -> Nothing
    Private -> Nothing
    _       -> Just m
  (PrivateMember, m) -> case m of
    Variable _ _ FunctionType{} _ _ _ -> Just m
    _ | hasDocs m                     -> Just m
      | otherwise                     -> Nothing

-------------------
-- Module forest --
-------------------

moduleForest :: String -> Cli.Format -> [[String]] -> Blocks
moduleForest ext fmt ms = case fmt of
    Html     -> styleBlocks htmlStyle <> nav (hdr <> mods)
    Markdown -> hdr <> mods
  where
    nav  = divWith ("nav", ["nav"], [])
    hdr  = header 1 "Modules"
    mods = blockHtml $ withPre $ drawPowerline $ (fmap.fmap) T.pack $ mkModuleForest ext ms

mkModuleForest :: String -> [[String]] -> [Tree String]
mkModuleForest ext = go [] . groupByHead
  where
    groupByHead :: [[String]] -> [[[String]]]
    groupByHead = groupBy ((==) `on` head)

    go :: [String]      -- ^ module prefix, stores seen node names
       -> [[[String]]]  -- ^ groups
       -> [Tree String]
    go _   []     = []
    go pfx (g:gs) = case g of
      (name:_):_ -> let g' = map tail g
                        node = if any null g' -- module exists
                                 then moduleLink pfx name
                                 else name
                        ns = groupByHead $ filter (/= mempty) g'
                    in Node node (go (pfx <> [name]) ns) : go pfx gs
      _          -> []

    moduleLink :: [String] -> String -> String
    moduleLink pfx name = "<a href=\"" <> mdUrl <> "\">" <> name <> "</a>"
      where
        mdUrl = Posix.joinPath (pfx <> [name]) <.> ext

-------------
-- Utility --
-------------

anchor :: MonadReader Env m => ExpSrc -> m (Doc SigAnn)
anchor e = do
    root <- asks sourceUrl
    path <- asks moduleIdentifier
    return (annotate (Anchor url name) section <> source root path)
  where
    name = T.pack $ hyphenateQualifiedName $ getQualifiedName e
    url  = "#" <> name
    section = "\x00A7" -- §
    source [] _ = mempty
    source root path = annotate (Source srcUrl) "source"
      where
        srcUrl = T.pack $ (<> lineNo (note $ unfix e)) $ addExtension (Posix.joinPath (root : path)) "k"
        lineNo (SrcStack a _) = lineNo a
        lineNo Src{..} = "#L" ++ show (unPos $ sourceLine begin)
        lineNo SrcUnknown = mempty

collapsible
  :: Inlines -- ^ summary
  -> Blocks  -- ^ details
  -> Blocks
collapsible summary details
  | details == mempty = mempty
  | otherwise = mconcat
    [ blockHtml "<details>"
    , B.para $ inlineHtml "<summary>" <> summary <> inlineHtml "</summary>"
    , B.para ""
    , details
    , blockHtml "</details>"
    ]

-- | 'collapsible', filtered 'subList'
cflist :: Inlines -> [Rendering] -> Blocks
cflist _     [] = mempty
cflist title rs
    | any ((mempty /=) . sanSig) rs = clist title rs
    | otherwise                     = mempty

-- | 'collapsible' 'subList'
clist :: Inlines -> [Rendering] -> Blocks
clist _     [] = mempty
clist title rs = collapsible title $ subList $ conSig <$> rs

describeDocs :: NotedExp Src e -> App Blocks
describeDocs e = case note $ unfix e of
  Src{ docs = (preDocs, postDocs) } ->
    (<>) <$> processDocs preDocs <*> processDocs postDocs
  _ -> mempty

hasDocs :: ExpSrc -> Bool
hasDocs = uncurry eitherNotNull . docs . note . unfix
  where
    eitherNotNull :: [a] -> [a] -> Bool
    eitherNotNull = (||) `on` notNull

getIdent :: ExpSrc -> Maybe Text
getIdent = \case
  StaticAssert{}   -> Nothing
  StaticIf{}       -> Nothing
  (ModuleDiff a _) -> Just $ T.pack $ intercalate "." a
  Extern _ b       -> getIdent b
  x                -> Just $ T.pack $ getName x

getQualifiedIdent :: ExpSrc -> Maybe QualifiedName
getQualifiedIdent = \case
  StaticAssert{} -> Nothing
  StaticIf{}     -> Nothing
  Extern _ b     -> getQualifiedIdent b
  x              -> Just $ getQualifiedName x

hyphenateQualifiedName :: [String] -> String
hyphenateQualifiedName = tail . map hyphen . intercalate "-"
  where
    hyphen '@' = '-'
    hyphen c   = c

linkPath :: MonadReader Env m => [String] -> m FilePath
linkPath qn = modulePath (head qn) <&> (<> "#" <> hyphenateQualifiedName qn)

-- | Posix filepath from current module to target module via root
modulePath
  :: MonadReader Env m
  => String -- ^ target module name: "@sample@links"
  -> m FilePath
modulePath target = do
  ext <- asks fileExt
  rootPath <&> (: splitModuleName target)
           <&> Posix.joinPath
           <&> flip addExtension ext

-- | Convert "@sample@decls" to ["sample", "decls"].
splitModuleName :: String -> [String]
splitModuleName = filter (/= mempty) . splitOn "@"

-- | Generate relative posix filepath to documentation root directory from
-- current supplied module name.
--
-- For example, if the module identifer is ["sample","decls"] then root path is
-- ".."
rootPath :: MonadReader Env m => m FilePath
rootPath = asks $ Posix.joinPath
                      . flip replicate ".."
                      . subtract 1
                      . length
                      . moduleIdentifier

currentPath :: MonadReader Env m => m [FilePath]
currentPath = (<>) <$> asks outputDirectory <*> asks moduleIdentifier

svgPath :: (MonadReader Env m, MonadState S m) => m FilePath
svgPath = do
  idx <- getSvgIdx
  flip addExtension (show idx <> ".svg") . Posix.joinPath <$> currentPath

blockHtml :: Text -> Blocks
blockHtml = rawBlock "html"

inlineHtml :: Text -> Inlines
inlineHtml = rawInline "html"

processDocs :: [DocComment] -> App Blocks
processDocs dcs = do
  hasSvgBob <- asks svgbob
  let chunks = map stripDocPrefix dcs
                 & unpackDocComment
                 & parseSvgbob
                 & groupEithers
  fmap mconcat $ forM chunks $ \case
    cs@(Left _ :| _) -> NE.toList cs
                               & lefts
                               & unlines
                               & T.pack
                               & readMarkdownPureUnsafe
                               & return
    cs -> do
      let t = unlines $ rights $ NE.toList cs
      if hasSvgBob
        then do
          rawSvg <- liftIO $ svgbobProcess t
          svgFilePath <- svgPath
          let svgURL = ("./" <>) $ T.pack $ takeFileName svgFilePath
          liftIO $ do
            createDirectoryIfMissing True $ takeDirectory svgFilePath
            writeFile svgFilePath rawSvg
          return $ plain $ image svgURL mempty mempty
        else return $ codeBlock $ T.pack t

unpackDocComment :: [DocComment] -> [String]
unpackDocComment = concatMap $ \case
  LineComment  lcls -> map removeMargin lcls
  BlockComment bcls -> bcls
  where
    removeMargin :: String -> String
    removeMargin [] = []
    removeMargin (' ':xs) = xs
    removeMargin xs = xs

groupEithers :: [Either a b] -> [NonEmpty (Either a b)]
groupEithers = NE.groupBy ((==) `on` isRight)

-- | Parse svgbob lines which start with `@@`. Lefts are regular strings,
-- rights are svgbob lines.
parseSvgbob :: [String] -> [Either String String]
parseSvgbob = labelLines False
  where
    labelLines _        []     = []
    labelLines isSvgbob (l:ls) = case dropWhile (' ' ==) l of
      '@':'@':xs | not isSvgbob -> Right xs : labelLines True  ls -- begin
                 | otherwise    -> Left  xs : labelLines False ls -- end
      _                         -> let h = if isSvgbob then Right else Left
                                   in h l : labelLines isSvgbob ls

svgbobProcess :: String -> IO String
svgbobProcess = fmap handleErr . try . readCreateProcess (proc "svgbob_cli" [])
  where
    handleErr :: Either SomeException String -> String
    handleErr = \case
      Right r -> r
      Left  l -> error $ unlines
        [ "`svgbob_cli` executable error. Check it is properly installed or \
          \disable `--svgbob` option."
        , displayException l
        ]

readMarkdownPureUnsafe :: Text -> Blocks
readMarkdownPureUnsafe = handleResult . runPure . readMarkdown opts
  where
    opts :: ReaderOptions
    opts = def{ readerExtensions = pandocExtensions }

    handleResult :: Either PandocError Pandoc -> Blocks
    handleResult = \case
        Right (Pandoc _ blks) -> fromList blks
        Left _ -> error "Cannot read Markdown."

stripDocPrefix :: DocComment -> DocComment
stripDocPrefix = \case
  LineComment  lcls -> LineComment  $ go lcls
  BlockComment bcls -> BlockComment $ go bcls
  where
    go :: [String] -> [String]
    go [] = []
    go (l:ls) = l' : ls
      where
        l' = tail $ dropWhile isSpace l

withPre :: Text -> Text
withPre t = "<p><pre>\n\n" <> t <> "</pre></p>"

pre :: Text -> App Blocks
pre t = asks format <&> \case
  Markdown -> mconcat
    [ blockHtml $ TL.toStrict $ mconcat
        [ "<pre style=\""
        , renderHtmlInline signatureStyle
        , "\">"
        ]
    , blockHtml t
    , blockHtml "</pre>"
    ]
  Html -> mconcat
    [ blockHtml "<pre class=\"signature\">"
    , blockHtml t
    , blockHtml "</pre>"
    ]

subList :: [Blocks] -> Blocks
subList [] = mempty
subList bs = divWith (mempty, ["subList"], []) $
               foldMap (divWith (mempty, ["subListItem"], [])) bs

subListToMd :: Block -> Block
subListToMd = \case
  Div (_, ["subList"], _) bs -> BulletList $ fromSubListItem <$> bs
  b                          -> b

fromSubListItem :: Block -> [Block]
fromSubListItem = \case
  Div (_, ["subListItem"], _) bs -> bs
  _                              -> error "Invalid subListItem"

subListToHtml :: Block -> Block
subListToHtml = \case
  Div (_, ["subList"], _) bs ->  Div nullAttr $ join $ wrapUl $ wrapLi . fromSubListItem <$> bs
  b -> b
  where
    wrapLi :: [Block] -> [Block]
    wrapLi bs = RawBlock "html" "<li>" : bs <> [RawBlock "html" "</li>"]
    wrapUl :: [[Block]] -> [[Block]]
    wrapUl bs = [RawBlock "html" "<ul class=\"subList\">"] : bs <> [[RawBlock "html" "</ul>"]]

attrList :: [Rendering] -> Doc SigAnn
attrList []    = mempty
attrList attrs = brackets (list $ map sig attrs) <> " "

withAttrList :: [Rendering] -> [Doc SigAnn] -> [Doc SigAnn]
withAttrList []    ds = ds
withAttrList attrs ds = (brackets . list) (sig <$> attrs) <> flatAlt "" " " : ds

getAnchors :: ExpSrc -> Set QualifiedName
getAnchors = S.fromList . map getQualifiedName . go
  where
    go e = case e of
      Alias{}             -> e : e'
      Class _ _ (Seq bdy) -> e : foldMap go (classMembers bdy)
      Enum{}              -> e : e'
      EnumConstant{}      -> e : e'
      Function{}          -> e : e'
      FunctionDecl{}      -> e : e'
      Struct{}            -> e : e'
      Union{}             -> e : e'
      Variable{}          -> e : e'
      _                   -> e'
      where
        e' = foldMap go $ unfix e
