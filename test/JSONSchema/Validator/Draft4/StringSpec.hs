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
