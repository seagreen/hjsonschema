
module Local.Reference where

import           Protolude

import           Test.Hspec

import           Data.Validator.Reference

spec :: Spec
spec = do
    it "updateResolutionScope gives correct results" $ do
        updateResolutionScope Nothing Nothing
            `shouldBe` Nothing

        updateResolutionScope Nothing (Just "#")
            `shouldBe` Nothing

        updateResolutionScope Nothing (Just "foo")
            `shouldBe` Just "foo"

        -- TODO: Normalize after updateResolutionScope:
        updateResolutionScope (Just "/foo") (Just "./bar")
            `shouldBe` Just "/./bar"

    it "resolveReference  gives correct results" $ do
        resolveReference (Just "/foo") "bar"
            `shouldBe` (Just "/bar", Nothing)

        resolveReference (Just "/foo/bar") "/baz"
            `shouldBe` (Just "/baz", Nothing)

        resolveReference Nothing "#/bar"
            `shouldBe` (Nothing, Just "/bar")

        resolveReference (Just "/foo") "#/bar"
            `shouldBe` (Just "/foo", Just "/bar")
