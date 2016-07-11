
module Local.Failure where

import           Prelude

import           Data.Aeson
import qualified Data.Aeson.Pointer          as P
import           Data.Monoid
import           Test.Tasty                  (TestTree)
import qualified Test.Tasty.HUnit            as HU

import           Data.JsonSchema.Draft4
import           Data.JsonSchema.Draft4.Spec (validate)
import qualified Data.Validator.Draft4.Array as AR

correctPaths :: [TestTree]
correctPaths =
    [ HU.testCase "Items object" itemsObject
    , HU.testCase "Items array" itemsArray
    ]

itemsObject :: IO ()
itemsObject =
    HU.assertEqual
        "Path to invalid data"
        [Failure (Items UniqueItems) (Bool True) (P.Pointer [P.Token "0"])
                 (toJSON [True, True])
        ]
        failures
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

itemsArray :: IO ()
itemsArray =
    HU.assertEqual
        "Path to invalid data"
        [Failure (Items UniqueItems) (Bool True) (P.Pointer [P.Token "0"])
                 (toJSON [True, True])
        ]
        failures
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
