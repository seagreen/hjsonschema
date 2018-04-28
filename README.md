# DEPRECATION NOTICE

`hjsonschema` was an attempt to build a very modular JSON Schema library. Validators have [a concrete type](src/JSONSchema/Validator/Types.hs) and can be mixed and matched into new [Specs](src/JSONSchema/Types.hs).

However this flexibility came at the price of complicating the code. I don't think it was the right tradeoff, especially since situations where you'd want to change what validators make up a `Spec` at runtime seem rare.

Also, there are many parts of JSON Schema that `hjsonschema` doesn't implement properly (as you can see from the issue tracker). I'm hoping that a new JSON Schema library will come along that handles these correctly. In the meantime I'm happy to merge working fixes into here.

# Links

[Hackage](https://hackage.haskell.org/package/hjsonschema) / [GitHub](https://github.com/seagreen/hjsonschema) / [Travis CI](https://travis-ci.org/seagreen/hjsonschema)

# Example

See [here](https://github.com/seagreen/hjsonschema/blob/master/examples/Simple.hs).

# System dependencies

+ Requires [pcre](http://www.pcre.org/) (`pkgs.pcre` in Nixpkgs).

## Vendoring

+ `JSON-Schema-Test-Suite` is vendored from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema-org/JSON-Schema-Test-Suite).

+ `src/draft4.json` is from commit # c1b12bf699f29a04b4286711c6e3bbfba66f21e5 [here](https://github.com/json-schema/json-schema). The [root ref in remote ref](./JSON-Schema-Test-Suite/tests/draft4/refRemote.json) test has been modified to fix [#175](https://github.com/json-schema-org/JSON-Schema-Test-Suite/issues/175).
