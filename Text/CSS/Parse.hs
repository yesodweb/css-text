{-# LANGUAGE OverloadedStrings #-}
module Text.CSS.Parse
    ( attrParser
    , attrsParser
    , blockParser
    , blocksParser
    , parseAttr
    , parseAttrs
    , parseBlock
    , parseBlocks
    , parseNestedBlocks
    , NestedBlock(..)
    ) where

import Prelude hiding (takeWhile, take)
import Data.Attoparsec.Text
import Data.Text (Text, strip)
import Control.Applicative ((<|>), many, (<$>))
import Data.Char (isSpace)

type CssBlock = (Text, [(Text, Text)])
data NestedBlock = NestedBlock Text [NestedBlock]
                 | LeafBlock CssBlock
                 deriving (Eq, Show)

-- | The preferred parser, will capture media queries
parseNestedBlocks :: Text -> Either String [NestedBlock]
parseNestedBlocks = parseOnly nestedBlocksParser

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
    skipWS
    sel <- takeWhile (/= '{')
    _ <- char '{'
    attrs <- attrsParser
    skipWS
    _ <- char '}'
    return (strip sel, attrs)

nestedBlockParser :: Parser NestedBlock
nestedBlockParser = do
    skipWS
    sel <- strip <$> takeTill (== '{')
    _ <- char '{'
    skipWS

    unknown <- strip <$> takeTill (\c -> c == '{' || c == '}' || c == ':')
    mc <- peekChar
    res <- case mc of
      Nothing -> fail "unexpected end of input"
      Just c -> nestedParse sel unknown c

    skipWS
    _ <- char '}'
    return res
  where
    -- no colon means no content
    nestedParse sel _ '}' = return $ LeafBlock (sel, [])

    nestedParse sel unknown ':' = do
        _ <- char ':'
        value <- valueParser
        (char ';' >> return ()) <|> return ()
        skipWS
        moreAttrs <- attrsParser
        return $ LeafBlock (sel, (unknown, strip value) : moreAttrs)

    -- TODO: handle infinite nesting
    nestedParse sel unknown '{' = do
        _ <- char '{'
        attrs <- attrsParser
        skipWS
        _ <- char '}'
        blocks <- blocksParser
        return $ NestedBlock sel $ map LeafBlock $ (unknown, attrs) : blocks
    nestedParse _ _ c = fail $ "expected { or : but got " ++ [c]

blocksParser :: Parser [(Text, [(Text, Text)])]
blocksParser = many blockParser

nestedBlocksParser :: Parser [NestedBlock]
nestedBlocksParser = many nestedBlockParser
