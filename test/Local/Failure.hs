
module Local.Failure where

import           Prelude

import           Data.Aeson
import qualified Data.Aeson.Pointer          as P
import           Data.Monoid
import           Test.Hspec

import           Data.JsonSchema.Draft4
import           Data.JsonSchema.Draft4.Spec (validate)
import qualified Data.Validator.Draft4.Array as AR

spec :: Spec
spec = do
    it "items array failures are constructed correctly" itemsArray
    it "items object failures are constructed correctly" itemsObject

itemsArray :: Expectation
itemsArray =
    failures `shouldBe`
        [Failure (Items UniqueItems) (Bool True) (P.Pointer [P.Token "0"])
                 (toJSON [True, True])
        ]
  where
    failures = validate (ReferencedSchemas schema mempty)
                        sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ AR.ItemsArray
            [emptySchema { _schemaUniqueItems = Just True }]
        }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
        { _swSchema = schema
        , _swURI    = Nothing
        }

itemsObject :: Expectation
itemsObject =
    failures `shouldBe`
        [Failure (Items UniqueItems) (Bool True) (P.Pointer [P.Token "0"])
                 (toJSON [True, True])
        ]
  where
    failures = validate (ReferencedSchemas schema mempty)
                        sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ AR.ItemsObject
            (emptySchema { _schemaUniqueItems = Just True })
        }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
        { _swSchema = schema
        , _swURI    = Nothing
        }
