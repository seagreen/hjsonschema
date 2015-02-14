# Intro

An implementation of [JSON Schema](http://json-schema.org/) v4 in haskell.

# Status

Still in development. Lacks documentation, example code, and real error messages.

Also while the official tests pass, its handling of the `id` keyword is incomplete.

# Install Tests

    git submodule update --init

# Run Tests

    cd JSON-Schema-Test-Suite/remotes
    python -m SimpleHTTPServer 1234

Then run the normal `cabal test` from another terminal.

Note that the tests require an internet connection.

# Notes

This uses the [regexpr](https://hackage.haskell.org/package/regexpr-0.5.4) regular expression library fo the "pattern" validator. I have no idea if this is compatible with the ECMA 262 regex dialect, which the [spec](http://json-schema.org/latest/json-schema-validation.html#anchor33) requires.

# Credits

Thanks to [Julian Berman](https://github.com/Julian) for the fantastic test suite.

Also thanks to Tim Baumann for his [aeson-schema](https://hackage.haskell.org/package/aeson-schema) library. Hjsonschema is based on Aeson-Schema, and some code is directly from there.
