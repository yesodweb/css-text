{-# LANGUAGE OverloadedStrings #-}
import Text.CSS.Parse
import Test.Hspec.Monadic
import Test.Hspec.HUnit
import Test.HUnit ((@=?))
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly)

main = hspecX $ do
    describe "single attribute parser" $ do
        it "trimming whitespace" $
            Right ("foo", "bar") @=? parseOnly parseAttr "   foo   : bar   "
    describe "multiple attribute parser" $ do
        it "no final semicolon" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseOnly parseAttrs " foo: bar ;  baz : bin  "
        it "final semicolon" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseOnly parseAttrs " foo: bar ;  baz : bin  ;"
        it "ignores comments" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseOnly parseAttrs " foo: bar ; /* ignored */ baz : bin  ;"
    describe "block parser" $ do
        it "multiple blocks" $
            Right [ ("foo", [("fooK1", "fooV1"), ("fooK2", "fooV2")])
            , ("bar", [("barK1", "barV1"), ("barK2", "barV2")])
            ] @=? parseOnly parseBlocks (T.concat
            [ "foo{fooK1:fooV1;/*ignored*/fooK2:fooV2               }\n\n"
            , "/*ignored*/"
            , "bar{barK1:barV1;/*ignored*/barK2:barV2               ;}\n\n/*ignored*/"
            ])
