# Intro

An implementation of [JSON Schema](http://json-schema.org/) Draft 4 in haskell.

# Status

Still in development. Lacks solid code to fetch remote schemas.

# Example

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.JsonSchema as JS
import qualified Data.Vector as V

main :: IO ()
main = do
  eitherGraph <- JS.fetchRefs JS.draft4 rawSchema H.empty
  case eitherGraph of
    Left e  -> print e
    Right g ->
      case JS.compileDraft4 g rawSchema of
        Left e2 -> print e2
        Right a -> print $ JS.validate a invalidData

rawSchema :: JS.RawSchema
rawSchema = JS.RawSchema
  { JS._rsURI = ""
  , JS._rsObject = H.singleton "uniqueItems" (Bool True) -- Schema JSON goes here.
  }

invalidData :: Value
invalidData = Array (V.fromList ["foo", "foo"])
```

Output:
```
fromList ["Val error against uniqueItems True for: Array (fromList [String \"foo\",String \"foo\"])"]
```


# Install Tests

    git submodule update --init

# Run Tests

    cd JSON-Schema-Test-Suite/remotes
    python -m SimpleHTTPServer 1234

Then run the normal `cabal test` from another terminal.

Note that the tests require an internet connection.

# Notes

+ This uses the [regexpr](https://hackage.haskell.org/package/regexpr-0.5.4) regular expression library fo the "pattern" validator. I have no idea if this is compatible with the ECMA 262 regex dialect, which the [spec](http://json-schema.org/latest/json-schema-validation.html#anchor33) requires.

+ `draft4.json` is from commit # cc8ec81ce0abe2385ebd6c2a6f2d6deb646f874a [here](https://github.com/json-schema/json-schema).

# Credits

Thanks to [Julian Berman](https://github.com/Julian) for the fantastic test suite.

Also thanks to Tim Baumann for his [aeson-schema](https://hackage.haskell.org/package/aeson-schema) library. Hjsonschema's test code and its implementation of `Graph` both come from Aeson-Schema.