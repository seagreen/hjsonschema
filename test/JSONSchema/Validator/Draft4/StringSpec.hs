module JSONSchema.Validator.Draft4.StringSpec (spec) where

import           Protolude

import           Test.Hspec

import           JSONSchema.Validator.Draft4.String

spec :: Spec
spec = do
    describe "maxLengthVal" $ do
        context "with 0" $ do
            it "accepts the empty string" $ do
                maxLengthVal (MaxLength 0) "" `shouldBe` Nothing

            it "rejects other strings" $ do
                maxLengthVal (MaxLength 0) "foo" `shouldBe` Just (MaxLengthInvalid (MaxLength 0) "foo")

    describe "patternVal" $ do
        it "matches a substring" $ do
            patternVal (PatternValidator "baz") "foo bar baz" `shouldBe` Nothing

        context "with .*" $ do
            it "matches the empty string (test for workaround for a bug in pcre-light)" $ do
                patternVal (PatternValidator ".*") "" `shouldBe` Nothing
