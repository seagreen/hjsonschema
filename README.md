# Intro

An implementation of [JSON Schema](http://json-schema.org/) Draft 4 in haskell.

# Status

Still in development. Lacks solid code to handle changing resolution scope.

Also note that hjsonschema uses the [regexpr](https://hackage.haskell.org/package/regexpr-0.5.4) regular expression library for the "pattern" validator. This isn't compatible with the ECMA 262 regex dialect, which the [spec](http://json-schema.org/latest/json-schema-validation.html#anchor33) requires.

# Example

See [Example.hs](./Example.hs).

# Install Tests

    git submodule update --init

# Notes

+ `draft4.json` is from commit # cc8ec81ce0abe2385ebd6c2a6f2d6deb646f874a [here](https://github.com/json-schema/json-schema).

# Credits

Thanks to [Julian Berman](https://github.com/Julian) for the fantastic test suite.

Also thanks to Tim Baumann for his [aeson-schema](https://hackage.haskell.org/package/aeson-schema) library. Hjsonschema's test code and its implementation of `Graph` both originally came from there.
