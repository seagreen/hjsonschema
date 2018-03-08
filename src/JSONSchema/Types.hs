module JSONSchema.Types where

import           Import

import           JSONSchema.Validator.Types (Validator(..))

newtype Spec schema err
    = Spec { _unSpec :: [Validator schema schema err] }

-- | Return a schema's immediate subschemas.
--
-- The first list is subschemas validating the same level of the document,
-- the second list is subschemas validating lower levels (see
-- 'JSONSchema.Validator.Types.Fail' for a full explanation).
embedded :: Spec schema a -> schema -> ([schema], [schema])
embedded spec schema =
    let embeds = (\val -> _embedded val schema) <$> _unSpec spec
    in foldl' (\(a,b) (x,y) -> (x <> a, y <> b)) (mempty, mempty) embeds

validate
    :: Spec schema err
    -> schema
    -> Value
    -> [err]
validate spec schema v =
    (\val -> _validate val schema v) =<< _unSpec spec

-- | A basic schema type that doesn't impose much structure.
--
-- 'JSONSchema.Draft4' doesn't use this, but instead uses the record based
-- one defined in 'JSONSchema.Draft4.Schema' to make it easier to write
-- draft 4 schemas in Haskell.
newtype Schema
    = Schema { _unSchema :: HashMap Text Value }
    deriving (Eq, Show, FromJSON, ToJSON)
