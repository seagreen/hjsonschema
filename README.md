![hjsonschema logo](./logo.jpg)

A Haskell implementation of the most commonly used [JSON Schema](http://json-schema.org/) specification ([Draft 4](https://github.com/json-schema-org/json-schema-spec/wiki/Specification-Links#draft-4)).

[Hackage](https://hackage.haskell.org/package/hjsonschema) / [GitHub](https://github.com/seagreen/hjsonschema) / [Travis CI](https://travis-ci.org/seagreen/hjsonschema)

# Notes

+ As of 2017 [json-schema-org](https://github.com/json-schema-org/json-schema-spec) has begun releasing new drafts of the standard. Once this work stabilizes `hjsonschema` will add coverage for the latest draft.

+ Requires [pcre](http://www.pcre.org/) (`pkgs.pcre` in Nixpkgs).

+ Schemas with circular references can cause infinite loops. hjsonschema does loop detection but it may not be solid yet -- please open an issue if you find a situation where it fails.

# Example

See [here](https://github.com/seagreen/hjsonschema/blob/master/examples/Simple.hs).

# Tests

Run all tests:

`stack test`

Run only local tests:

`stack test hjsonschema:local`

Run remote tests (makes GETs to json-schema.org, also temporarily starts an HTTP server on port 1234):

`stack test hjsonschema:remote`

# Details

## Goals

+ Be a correct and fast implementation of the spec.

+ Be a useful reference for implementers in other languages. Haskell's high level nature, expressive type system and referential transparency suit this purpose well.

## Good Parts

+ Passes all the required tests in the [language agnostic test suite](https://github.com/json-schema/JSON-Schema-Test-Suite). NOTE: due to an issue with the test suite this isn't true at the moment, see [#175](https://github.com/json-schema-org/JSON-Schema-Test-Suite/issues/175).

+ Very modular, which should make it easy to support future versions of the specification.

## Bad Parts

+ Uses the [pcre-heavy](https://hackage.haskell.org/package/pcre-heavy) regular expression library for the "pattern" validator. It should use a library based on the ECMA 262 regex dialect, which the [spec](http://json-schema.org/latest/json-schema-validation.html#anchor33) requires.

+ Currently doesn't support the optional `"format"` validators.

## Vendoring

+ `JSON-Schema-Test-Suite` is vendored from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema-org/JSON-Schema-Test-Suite).

+ `src/draft4.json` is from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema/json-schema). The [root ref in remote ref](./JSON-Schema-Test-Suite/tests/draft4/refRemote.json) test has been modified to fix [#175](https://github.com/json-schema-org/JSON-Schema-Test-Suite/issues/175).

## Credits

[TJ Weigel](http://tjweigel.com/) created the logo.

[Tim Baumann](https://github.com/timjb) wrote [aeson-schema](https://hackage.haskell.org/package/aeson-schema), on which hjsonschema's test code and its implementation of `SchemaGraph` were based.

[Julian Berman](https://github.com/Julian) maintains the fantastic [language agnostic test suite](https://github.com/json-schema/JSON-Schema-Test-Suite).
