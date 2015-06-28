{-# LANGUAGE OverloadedStrings #-}
-- | Parse CSS with parseNestedBlocks and render it with renderNestedBlock
module Text.CSS.Parse
    ( NestedBlock(..)
    , parseNestedBlocks
    , parseBlocks
    , parseBlock
    , attrParser
    , attrsParser
    , blockParser
    , blocksParser
    , parseAttr
    , parseAttrs
    ) where

import Prelude hiding (takeWhile, take)
import Data.Attoparsec.Text
import Data.Text (Text, strip, append)
import Control.Applicative ((<|>), many, (<$>))
import Data.Char (isSpace)
import Control.Monad (mzero)

type CssBlock = (Text, [(Text, Text)])
data NestedBlock = NestedBlock Text [NestedBlock] -- ^ for example a media query
                 | LeafBlock CssBlock
                 deriving (Eq, Show)

-- | The preferred parser, will capture media queries
parseNestedBlocks :: Text -> Either String [NestedBlock]
parseNestedBlocks = parseOnly nestedBlocksParser

-- | The original parser of basic CSS, but throws out media queries
parseBlocks :: Text -> Either String [CssBlock]
parseBlocks = parseOnly blocksParser

parseBlock :: Text -> Either String CssBlock
parseBlock = parseOnly blockParser

parseAttrs :: Text -> Either String [(Text, Text)]
parseAttrs = parseOnly attrsParser

parseAttr :: Text -> Either String (Text, Text)
parseAttr = parseOnly attrParser


skipWS :: Parser ()
skipWS = (string "/*" >> endComment >> skipWS)
     <|> (skip isSpace >> skipWhile isSpace >> skipWS)
     <|> return ()
  where
    endComment = do
        skipWhile (/= '*')
        (do
            _ <- char '*'
            (char '/' >> return ()) <|> endComment
            ) <|> fail "Missing end comment"

attrParser :: Parser (Text, Text)
attrParser = do
    skipWS
    key <- takeWhile1 (\c -> c /= ':' && c /= '{' && c /= '}')
    _ <- char ':' <|> fail "Missing colon in attribute"
    value <- valueParser
    return (strip key, strip value)

valueParser :: Parser Text
valueParser = takeWhile (\c -> c /= ';' && c /= '}')

attrsParser :: Parser [(Text, Text)]
attrsParser = (do
    a <- attrParser
    (char ';' >> skipWS >> ((a :) <$> attrsParser))
      <|> return [a]
  ) <|> return []

blockParser :: Parser (Text, [(Text, Text)])
blockParser = do
    sel <- takeWhile (/= '{')
    _ <- char '{'
    attrs <- attrsParser
    skipWS
    _ <- char '}'
    return (strip sel, attrs)

mediaQueryParser :: Parser NestedBlock
mediaQueryParser = do
    _ <- char '@'
    sel <- strip <$> takeWhile (/= '{')
    _ <- char '{'
    blocks <- nestedBlocksParser
    skipWS
    _ <- char '}'
    return $ NestedBlock ("@" `append` sel) $ blocks

blocksParser :: Parser [(Text, [(Text, Text)])]
blocksParser =  skipWS *> blockParser `sepBy` skipWS <* skipWS

nestedBlockParser :: Parser NestedBlock
nestedBlockParser = do
    mc <- peekChar
    case mc of
         Just '}' -> mzero
         _        -> mediaQueryParser <|> (LeafBlock <$> blockParser)

nestedBlocksParser :: Parser [NestedBlock]
nestedBlocksParser = skipWS *> nestedBlockParser `sepBy` skipWS <* skipWS
