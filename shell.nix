{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    pcre

    git # for `git:` references in stack.yml.
    pkgconfig
    zlib
  ];
}
