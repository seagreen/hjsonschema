# Summary

A Haskell implementation of [JSON Schema](http://json-schema.org/) ([Draft 4](http://json-schema.org/specification-links.html)).

[Hackage](https://hackage.haskell.org/package/hjsonschema) / [GitHub](https://github.com/seagreen/hjsonschema) / [Travis CI](https://travis-ci.org/seagreen/hjsonschema)

# Example

See [here](https://github.com/seagreen/hjsonschema/blob/master/examples/Simple.hs).

# Design

`hjsonschema` was an attempt to build a very modular JSON Schema library. Validators have [a concrete type](src/JSONSchema/Validator/Types.hs) and can be mixed and matched into new [Specs](src/JSONSchema/Types.hs).

However this flexibility comes at the price of complicating the code. I'm no longer sure it was the right tradeoff, especially since situations where you'd want to change what validators make up a `Spec` at runtime seem rare.

Also, there are edge cases of JSON Schema that `hjsonschema` doesn't implement properly (as you can see from the issue tracker). My motivation to fix them myself has ended. However, I'll still maintain the library, give feedback on issues, and merge PRs. I'd also be happy to advise any Haskellers who are interested in writing their own JSON Schema libraries.

# System dependencies

+ Requires [pcre](http://www.pcre.org/) (`pkgs.pcre` in Nixpkgs).

# Tests

Run all: `stack test`

Run only local tests: `stack test hjsonschema:local`

Run only remote tests (temporarily starts an HTTP server on port 1234 and makes GETs to json-schema.org): `stack test hjsonschema:remote`

## Vendoring

+ `JSON-Schema-Test-Suite` is vendored from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema-org/JSON-Schema-Test-Suite).

+ `src/draft4.json` is from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema/json-schema). The [root ref in remote ref](./JSON-Schema-Test-Suite/tests/draft4/refRemote.json) test has been modified to fix [#175](https://github.com/json-schema-org/JSON-Schema-Test-Suite/issues/175).

+ `.travis.yml` was created with `make_travis_yml_2.hs` commit # ea6c7d177a97bfbfb2fdc4deba943d60d2aff199.
