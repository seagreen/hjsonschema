
module Local.Failure where

import           Data.Aeson
import qualified Data.Aeson.Pointer     as P
import           Data.Monoid
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU

import           Data.JsonSchema.Draft4
import qualified Data.Validator.Draft4  as VA

correctPaths :: [TestTree]
correctPaths =
  [ HU.testCase "Items object" itemsObject
  , HU.testCase "Items array" itemsArray
  ]

itemsObject :: IO ()
itemsObject = HU.assertEqual "Path to invalid data"
                             (P.Pointer [P.Token "0"])
                             (_failureOffendingData failure)
  where
    [failure] = runValidate (ReferencedSchemas schema mempty)
                            sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsObject (emptySchema { _schemaUniqueItems = Just True }))
      }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
      { _swSchema = schema
      , _swURI    = Nothing
      }

itemsArray :: IO ()
itemsArray = HU.assertEqual "Path to invalid data"
                            (P.Pointer [P.Token "0"])
                            (_failureOffendingData failure)
  where
    [failure] = runValidate (ReferencedSchemas schema mempty)
                            sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsArray [emptySchema { _schemaUniqueItems = Just True }])
      }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
      { _swSchema = schema
      , _swURI    = Nothing
      }
