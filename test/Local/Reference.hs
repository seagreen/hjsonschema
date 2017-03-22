
module Local.Reference where

import           Protolude

import           Test.Hspec

import           JSONSchema.Validator.Reference

spec :: Spec
spec = do
    it "updateResolutionScope gives correct results" $ do
        updateResolutionScope (BaseURI Nothing) Nothing
            `shouldBe` BaseURI Nothing

        updateResolutionScope (BaseURI Nothing) (Just "#")
            `shouldBe` BaseURI Nothing

        updateResolutionScope (BaseURI Nothing) (Just "foo")
            `shouldBe` BaseURI (Just "foo")

        -- TODO: Normalize after updateResolutionScope:
        updateResolutionScope (BaseURI (Just "/foo")) (Just "./bar")
            `shouldBe` BaseURI (Just "/./bar")

    it "resolveReference  gives correct results" $ do
        resolveReference (BaseURI (Just "/foo")) "bar"
            `shouldBe` (Just "/bar", Nothing)

        resolveReference (BaseURI (Just "/foo/bar")) "/baz"
            `shouldBe` (Just "/baz", Nothing)

        resolveReference (BaseURI Nothing) "#/bar"
            `shouldBe` (Nothing, Just "/bar")

        resolveReference (BaseURI (Just "/foo")) "#/bar"
            `shouldBe` (Just "/foo", Just "/bar")
