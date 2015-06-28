{-# LANGUAGE OverloadedStrings #-}
import Text.CSS.Parse
import Text.CSS.Render
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Text (Text)
import Test.QuickCheck
import Control.Arrow ((***))
import Control.Monad (liftM)

main :: IO ()
main = hspec $ do
  describe "single attribute parser" $ do
    it "trimming whitespace" $
      parseAttr "   foo   : bar   " `shouldBe` Right ("foo", "bar")

  describe "multiple attribute parser" $ do
    it "no final semicolon" $
      parseAttrs " foo: bar ;  baz : bin  "
          `shouldBe` Right [("foo", "bar"), ("baz", "bin")]

    it "final semicolon" $
      parseAttrs " foo: bar ;  baz : bin  ;"
          `shouldBe` Right [("foo", "bar"), ("baz", "bin")]

    it "ignores comments" $
      parseAttrs " foo: bar ; /* ignored */ baz : bin  ;"
          `shouldBe` Right [("foo", "bar"), ("baz", "bin")]

  describe "block parser" $ do
    it "multiple blocks" $
      parseBlocks (T.concat
      [ "foo{fooK1:fooV1;/*ignored*/fooK2:fooV2               }\n\n"
      , "/*ignored*/"
      , "bar{barK1:barV1;/*ignored*/barK2:barV2               ;}\n\n/*ignored*/"
      ]) `shouldBe` Right [
        ("foo", [("fooK1", "fooV1"), ("fooK2", "fooV2")])
      , ("bar", [("barK1", "barV1"), ("barK2", "barV2")])
      ]

    it "media queries" $ do
      parseBlocks "@media print {* {text-shadow: none !important;} }"
        `shouldBe` Right []
      parseNestedBlocks "@media print {* {text-shadow: none !important; color: #000 !important; } a, a:visited { text-decoration: underline; }}"
        `shouldBe` Right [NestedBlock "@media print"
            [ LeafBlock ("*", [("text-shadow", "none !important"), ("color", "#000 !important")])
            , LeafBlock ("a, a:visited", [("text-decoration", "underline")])
            ]
          ]

  describe "render" $ -- do
    it "works" $
      renderBlocks [
            ("foo", [("bar", "baz"), ("bin", "bang")])
          , ("foo2", [("x", "y")])
          ]
          `shouldBe` "foo{bar:baz;bin:bang}foo2{x:y}"

  describe "parse/render" $ do
    prop "idempotent blocks" $ \bs ->
      parseBlocks (toStrict $ toLazyText $ renderBlocks $ unBlocks bs) == Right (unBlocks bs)
    prop "idempotent nested blocks" $ \bs ->
      parseNestedBlocks (toStrict $ toLazyText $ renderNestedBlocks bs) == Right bs

newtype Blocks = Blocks { unBlocks :: [(Text, [(Text, Text)])] }
    deriving (Show, Eq)

instance Arbitrary NestedBlock where
    arbitrary = resize 4 $ frequency
      [ (80, (LeafBlock . unBlock) `liftM` arbitrary)
      , (10, do mediatype <- elements ["@print", "@screen",
                                       "@media (min-width:768px)",
                                       "@media screen and (max-width: 300px)"]
                contents <- arbitrary
                return (NestedBlock mediatype contents))
      ]

instance Arbitrary Blocks where
    arbitrary = fmap (Blocks . map unBlock) arbitrary

newtype Block = Block { unBlock :: (Text, [(Text, Text)]) }
    deriving (Show, Eq)

instance Arbitrary Block where
    arbitrary = do
        sel <- frequency [
                     (90, unT <$> arbitrary)
                   , (10, return "@font-face")
                   ]
        attrs <- arbitrary
        return $ Block (sel, unAttrs attrs)

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
