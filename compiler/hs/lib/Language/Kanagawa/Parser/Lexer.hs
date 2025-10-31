{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Parser.Lexer
    ( symbol
    , lexeme
    , name
    , dotSepName
    , stringLiteral
    , integer
    , L.decimal
    , intSuffix
    , float
    , keyword
    , keyword2
    , operator
    , lparen
    , langle
    , rangle
    , rparen
    , parens
    , braces
    , angles
    , brackets
    , doubleBrackets
    , semi
    , comma
    , colon
    , dot
    , equal
    , hash
    , Config(..)
    , whitespace
    , docCommentPre
    , docCommentPost
    , docCommentPrefixPre
    , docCommentPrefixPost
    , skipBraces
    ) where

import Control.Monad (void)
import Control.Monad.Reader
import qualified Data.Char as Char
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)
import Language.Kanagawa.Parser.Syntax

docCommentPrefixPre, docCommentPrefixPost :: Char
docCommentPrefixPre  = '|'
docCommentPrefixPost = '<'

data Config =
    Config
    { parseDocs :: Bool
    , skipMemberBody :: Bool
    , skipClassBody :: Bool
    }
    deriving (Show, Eq)

docComment :: (MonadParsec e String m, MonadReader Config m) => Char -> m [DocComment]
docComment prefix = do
    docs <- asks parseDocs
    if docs then lexeme docComment' <?> "documentation comment"
            else return []
  where
    docComment' = do
        hasDocPrefix <- isJust <$> optional (lookAhead $ try ((string "//" <|> string "/*") *> hspace *> char prefix))
        if hasDocPrefix
          then many $ (LineComment <$> some lineComment) <|> (BlockComment . lines <$> blockComment)
          else return mempty

    lineComment  = string "//" *> manyTill anySingle (char   '\n') <* hspace
    blockComment = string "/*" *> manyTill anySingle (string "*/") <* hspace

docCommentPre, docCommentPost :: (MonadParsec e String m, MonadReader Config m) => m [DocComment]
docCommentPre = docComment docCommentPrefixPre
docCommentPost = docComment docCommentPrefixPost

whitespace :: (MonadParsec e String m, MonadReader Config m) => m ()
whitespace = do
    docs <- asks parseDocs
    if docs then whitespaceNonDoc
            else whitespaceAll
  where
    whitespaceAll = L.space space1 lineCmnt blockCmnt
      where
        lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockCommentNested "/*" "*/"

    whitespaceNonDoc = L.space space1 lineCmnt blockCmnt
      where
        lineCmnt  = skipNonDocLineComment "//"
        blockCmnt = skipNonDocBlockCommentNested "/*" "*/"

        startNonDocComment prefix =
            try (string prefix *> hspace *> notFollowedBy (char docCommentPrefixPre <|> char docCommentPrefixPost))

        skipNonDocLineComment prefix =
            startNonDocComment prefix >> void (takeWhileP (Just "character") (/= '\n'))

        skipNonDocBlockCommentNested start end =
            startNonDocComment start >> void (manyTill skip (string end))
          where
            skip = L.skipBlockCommentNested start end <|> void anySingle

skipBraces :: (MonadParsec e String m, MonadReader Config m) => m ()
skipBraces = lexeme $ L.skipBlockCommentNested "{" "}"

lexeme :: (MonadParsec e String m, MonadReader Config m) => m a -> m a
lexeme = L.lexeme whitespace

symbol :: (MonadParsec e String m, MonadReader Config m) => String -> m String
symbol = L.symbol whitespace

isBinDigit :: Char -> Bool
isBinDigit w = w == '0' || w == '1'

integer :: (Integral a, MonadParsec e String m) => m a
integer = (try hexadecimal <|> try binary <|> try octal <|> decimal) <?> "integer"
  where
    prefix x = char '0' >> char' x >> optional (char '_')

    binary      = prefix 'b' >> integerRadix 2 isBinDigit
    octal       = prefix 'o' >> integerRadix 8 Char.isOctDigit
    hexadecimal = prefix 'x' >> integerRadix 16 Char.isHexDigit
    decimal     = integerRadix 10 Char.isDigit

    integerRadix :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Num a)
            => a
            -> (Char -> Bool)
            -> m a
    integerRadix radix isDigit = do
        xs <- tokenList <$> takeWhile1P Nothing isDigit
        ys <- tokenList <$> takeWhileP Nothing isDigitOrUnderscore
        return $ foldl' step 0 $ xs <> ys
      where
        tokenList = chunkToTokens (Proxy :: Proxy s)
        step a w
            | w == '_'  = a
            | otherwise = a * radix + fromIntegral (Char.digitToInt w)
        isDigitOrUnderscore w = w == '_' || isDigit w

float :: (RealFloat a, MonadParsec e String m, MonadReader Config m) => m a
float = lexeme L.float

stringLiteral :: (MonadParsec e String m, MonadReader Config m, MonadReader Config m) => m String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

reserved :: Set String
reserved = Set.fromList
    [ "as"
    , "atomic"
    , "auto"
    , "barrier"
    , "bitsizeof"
    , "bitoffsetof"
    , "bool"
    , "break"
    , "bytesizeof"
    , "byteoffsetof"
    , "cast"
    , "case"
    , "class"
    , "clog2"
    , "concat"
    , "const"
    , "decltype"
    , "default"
    , "do"
    , "else"
    , "enum"
    , "export"
    , "extern"
    , "false"
    , "fan_out"
    , "float32"
    , "for"
    , "if"
    , "import"
    , "inline"
    , "int"
    , "lutmul"
    , "module"
    , "mux"
    , "noinline"
    , "private"
    , "public"
    , "reorder"
    , "return"
    , "static"
    , "static_assert"
    , "string"
    , "struct"
    , "switch"
    , "template"
    , "true"
    , "typename"
    , "uint"
    , "union"
    , "unrolled_for"
    , "using"
    , "void"
    , "while"
    ]

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore c = Char.isAlphaNum c || c == '_'

ident' :: MonadParsec e String m => (Char -> Bool) -> m String
ident' f = (:) <$> (letterChar <|> char '_') <*> takeWhileP (Just "rest of identifier") f

ident :: MonadParsec e String m => m String
ident = ident' isAlphaNumOrUnderscore

dotSepName :: (MonadParsec e String m, MonadReader Config m) => m [String]
dotSepName = lexeme $ try (ident' isAlphaNumOrDashOrUnderscore `sepBy1` char '.')
  where
    isAlphaNumOrDashOrUnderscore c = isAlphaNumOrUnderscore c || c == '-'

name :: (MonadParsec e String m, MonadReader Config m) => m Name
name = fromString <$> (lexeme . try) (ident >>= check) <?> "identifier"
  where
    check x = if x `Set.member` reserved || intType x
                then unexpected . Label . NE.fromList $ ("keyword " ++ show x)
                else return x
    intType ('i':'n':'t':x) = isJust (readMaybe x :: Maybe Word)
    intType ('u':'i':'n':'t':x) = isJust (readMaybe x :: Maybe Word)
    intType _ = False

keyword' :: (MonadParsec e String m, MonadReader Config m) => String -> m ()
keyword' s = void $ lexeme $ try (string s <* notFollowedBy (alphaNumChar <|> char '_'))

keyword :: (MonadParsec e String m, MonadReader Config m) => String -> m ()
keyword k = check >> keyword' k
  where
    check = if k `Set.member` reserved
                then return ()
                else error $ "keyword " ++ k ++ " is not declared in reserved list"

keyword2 :: (MonadParsec e String m, MonadReader Config m) => String -> String -> m ()
keyword2 k1 k2 = try (keyword k1 >> keyword' k2)

intSuffix :: (MonadParsec e String m, MonadReader Config m) => m Int
intSuffix  = lexeme (L.decimal <* notFollowedBy (alphaNumChar <|> char '_'))

operator :: (MonadParsec e String m, MonadReader Config m) => String -> m ()
operator o = (lexeme . try) (string o *> notFollowedBy (oneOf (o ++ "=")))

lparen, rparen :: (MonadParsec e String m, MonadReader Config m) => m String
lparen  = symbol "("
rparen = symbol ")"

langle, rangle :: (MonadParsec e String m, MonadReader Config m) => m ()
langle = _angle "<"
rangle = _angle ">"

_angle :: (MonadReader Config m, MonadParsec e String m) => String -> m ()
_angle o = lexeme $ try $ string o *> notFollowedBy (char '=')

parens, braces, angles, brackets, doubleBrackets :: (MonadParsec e String m, MonadReader Config m) => m a -> m a
parens   = between lparen rparen
braces   = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
doubleBrackets = brackets . brackets
angles   = between langle rangle

semi, comma, colon, dot, equal, hash :: (MonadParsec e String m, MonadReader Config m) => m String
semi    = symbol ";"
comma   = symbol ","
colon   = symbol ":"
dot     = symbol "."
equal   = symbol "="
hash    = symbol "#"
