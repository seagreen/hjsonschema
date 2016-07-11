with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  ghc = haskell.compiler.ghc801;
  buildInputs = [
    zlib.dev
    zlib.out
    pkgconfig
    pcre
  ];
}
