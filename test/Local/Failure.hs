
module Local.Failure where

import           Protolude

import           Data.Aeson
import           Test.Hspec
import qualified Data.Vector                 as V
import qualified JSONPointer                 as JP

import           JSONSchema.Draft4
import qualified JSONSchema.Draft4.Spec      as Spec
import qualified JSONSchema.Validator.Draft4 as VAL

spec :: Spec
spec = do
    it "items array failures are constructed correctly" itemsArray
    it "items object failures are constructed correctly" itemsObject
    it "reports infinitely looping references correctly" loopRef

itemsArray :: Expectation
itemsArray =
    failures `shouldBe`
        [ FailureItems (VAL.ItemsArrayInvalid (
            pure ( JP.Index 0
                 , pure (FailureUniqueItems (VAL.UniqueItemsInvalid
                       (V.fromList [Null, Null])
                   ))
                 )
          ))
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate mempty sw badData

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ VAL.ItemsArray
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
        [ FailureItems (VAL.ItemsObjectInvalid (
            pure ( JP.Index 1
                 , pure (FailureUniqueItems (VAL.UniqueItemsInvalid
                       (V.fromList [Null, Null])
                   ))
                 )
          ))
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate mempty sw badData

    schema :: Schema
    schema = emptySchema
        { _schemaItems = Just $ VAL.ItemsObject
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
        [ FailureRef (VAL.RefLoop
            "#"
            (VAL.VisitedSchemas [(Nothing, Nothing)])
            (Nothing, Nothing)
          )
        ]
  where
    failures :: [ValidatorFailure]
    failures = Spec.specValidate mempty sw badData

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
