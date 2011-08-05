{-# LANGUAGE OverloadedStrings #-}
module Text.CSS.Parse
    ( parseAttr
    , parseAttrs
    , parseBlock
    , parseBlocks
    ) where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.Text (Text, strip)
import Data.Char (isSpace)
import Control.Applicative ((<|>))

skipWS :: Parser ()
skipWS = (string "/*" >> endComment >> skipWS)
     <|> (satisfy isSpace >> skipSpace >> skipWS)
     <|> return ()
  where
    endComment = do
        skipWhile (/= '*')
        (do
            _ <- char '*'
            (char '/' >> return ()) <|> endComment
            ) <|> fail "Missing end comment"

parseAttr :: Parser (Text, Text)
parseAttr = do
    skipWS
    key <- takeWhile1 (not . flip elem ":{}")
    _ <- char ':' <|> fail "Missing colon in attribute"
    value <- (takeWhile (not . flip elem ";}"))
    return (strip key, strip value)

parseAttrs :: Parser [(Text, Text)]
parseAttrs =
    go id
  where
    go front = (do
        a <- parseAttr
        (char ';' >> return ()) <|> return ()
        skipWS
        go $ front . (:) a
        ) <|> return (front [])

parseBlock :: Parser (Text, [(Text, Text)])
parseBlock = do
    skipWS
    sel <- takeWhile (/= '{')
    _ <- char '{'
    attrs <- parseAttrs
    skipWS
    _ <- char '}'
    return (strip sel, attrs)

parseBlocks :: Parser [(Text, [(Text, Text)])]
parseBlocks = many parseBlock
