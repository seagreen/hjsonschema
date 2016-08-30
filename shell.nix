with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  ghc = haskell.compiler.ghc801;
  buildInputs = [
    pcre
  ];
}
