
module Local.Failure where

import           Protolude

import           Data.Aeson
import           Test.Hspec
import qualified Data.Vector                 as V
import qualified JSONPointer                 as JP

import           Data.JsonSchema.Draft4
import qualified Data.JsonSchema.Draft4.Spec as Spec
import qualified Data.Validator.Draft4       as D4

spec :: Spec
spec = do
    it "items array failures are constructed correctly" itemsArray
    it "items object failures are constructed correctly" itemsObject
    it "reports infinitely looping references correctly" loopRef

itemsArray :: Expectation
itemsArray =
    failures `shouldBe`
        [ FailureItems (D4.ItemsArrayInvalid (
            pure ( JP.Index 0
                 , pure (FailureUniqueItems (D4.UniqueItemsInvalid
                       (V.fromList [Null, Null])
                   ))
                 )
          ))
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate (ReferencedSchemas schema mempty) sw badData

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ D4.ItemsArray
            [emptySchema { _schemaUniqueItems = Just True }]
        }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
        { _swSchema = schema
        , _swURI    = Nothing
        }

    badData :: Value
    badData = toJSON [[Null, Null]]

itemsObject :: Expectation
itemsObject =
    failures `shouldBe`
        [ FailureItems (D4.ItemsObjectInvalid (
            pure ( JP.Index 1
                 , pure (FailureUniqueItems (D4.UniqueItemsInvalid
                       (V.fromList [Null, Null])
                   ))
                 )
          ))
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate (ReferencedSchemas schema mempty) sw badData

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ D4.ItemsObject
            (emptySchema { _schemaUniqueItems = Just True })
        }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
        { _swSchema = schema
        , _swURI    = Nothing
        }

    badData :: Value
    badData = toJSON [Null, toJSON [Null, Null]]

loopRef :: Expectation
loopRef =
    failures `shouldBe`
        [ FailureRef (D4.RefLoop
            "#"
            (D4.VisitedSchemas [(Nothing, Nothing)])
            (Nothing, Nothing)
          )
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate (ReferencedSchemas schema mempty) sw badData

    schema :: Schema
    schema = emptySchema
        { _schemaRef = Just "#"
        }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
        { _swSchema = schema
        , _swURI    = Nothing
        }

    badData :: Value
    badData = toJSON [[Null, Null]]
