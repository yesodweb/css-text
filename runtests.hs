{-# LANGUAGE OverloadedStrings #-}
import Text.CSS.Parse
import Text.CSS.Render
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import Test.HUnit ((@=?))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Text (Text)
import Test.QuickCheck
import Control.Arrow ((***))

main = hspecX $ do
    describe "single attribute parser" $ do
        it "trimming whitespace" $
            Right ("foo", "bar") @=? parseAttr "   foo   : bar   "
    describe "multiple attribute parser" $ do
        it "no final semicolon" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseAttrs " foo: bar ;  baz : bin  "
        it "final semicolon" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseAttrs " foo: bar ;  baz : bin  ;"
        it "ignores comments" $
            Right [("foo", "bar"), ("baz", "bin")] @=?
                parseAttrs " foo: bar ; /* ignored */ baz : bin  ;"
    describe "block parser" $ do
        it "multiple blocks" $
            Right [ ("foo", [("fooK1", "fooV1"), ("fooK2", "fooV2")])
            , ("bar", [("barK1", "barV1"), ("barK2", "barV2")])
            ] @=? parseBlocks (T.concat
            [ "foo{fooK1:fooV1;/*ignored*/fooK2:fooV2               }\n\n"
            , "/*ignored*/"
            , "bar{barK1:barV1;/*ignored*/barK2:barV2               ;}\n\n/*ignored*/"
            ])

    describe "render" $ do
        it "works" $
            "foo{bar:baz;bin:bang}foo2{x:y}" @=? renderBlocks
                [ ("foo", [("bar", "baz"), ("bin", "bang")])
                , ("foo2", [("x", "y")])
                ]

    describe "parse/render" $ do
        prop "is idempotent" $ \bs ->
            parseBlocks (toStrict $ toLazyText $ renderBlocks $ unBlocks bs) == Right (unBlocks bs)

newtype Blocks = Blocks { unBlocks :: [(Text, [(Text, Text)])] }
    deriving (Show, Eq)

instance Arbitrary Blocks where
    arbitrary = fmap (Blocks . map unBlock) arbitrary

newtype Block = Block { unBlock :: (Text, [(Text, Text)]) }
    deriving (Show, Eq)

instance Arbitrary Block where
    arbitrary = do
        (sel, attrs) <- arbitrary
        return $ Block (unT sel, unAttrs attrs)

newtype Attrs = Attrs { unAttrs :: [(Text, Text)] }

instance Arbitrary Attrs where
    arbitrary = fmap (Attrs . map (unT *** unT)) arbitrary

newtype T = T { unT :: Text }

instance Arbitrary T where
    arbitrary = fmap (T . T.pack) $ listOf1 $ elements $ concat
        [ ['A'..'Z']
        , ['a'..'z']
        , ['0'..'9']
        , "-_"
        ]
