{-# LANGUAGE OverloadedStrings #-}
module Text.CSS.Render
    ( renderAttr
    , renderAttrs
    , renderBlock
    , renderBlocks
    , renderNestedBlocks
    ) where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromText, singleton)
import Data.Monoid (mappend, mempty, mconcat)
import Text.CSS.Parse

(<>) :: Builder -> Builder -> Builder
(<>) = mappend

renderAttr :: (Text, Text) -> Builder
renderAttr (k, v) = fromText k <> singleton ':' <> fromText v

renderAttrs :: [(Text, Text)] -> Builder
renderAttrs [] = mempty
renderAttrs [x] = renderAttr x
renderAttrs (x:xs) = renderAttr x <> singleton ';' <> renderAttrs xs

renderBlock :: (Text, [(Text, Text)]) -> Builder
renderBlock (sel, attrs) =
    fromText sel <> singleton '{' <> renderAttrs attrs <> singleton '}'

renderBlocks :: [(Text, [(Text, Text)])] -> Builder
renderBlocks = mconcat . map renderBlock

renderNestedBlock :: NestedBlock -> Builder
renderNestedBlock (LeafBlock b) = renderBlock b
renderNestedBlock (NestedBlock t bs) = fromText t
                                    <> singleton '{'
                                    <> renderNestedBlocks bs 
                                    <> singleton '}'

renderNestedBlocks :: [NestedBlock] -> Builder
renderNestedBlocks = mconcat . map renderNestedBlock
