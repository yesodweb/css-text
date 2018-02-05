{-# LANGUAGE OverloadedStrings #-}
-- | Prender CSS with renderNestedBlocks
module Text.CSS.Render
    ( renderNestedBlocks
    , renderBlocks
    , renderBlock
    , renderAttrs
    , renderAttr
    ) where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromText, singleton)
import Data.Monoid (mempty, mconcat)
import Data.Semigroup ((<>))
import Text.CSS.Parse

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
