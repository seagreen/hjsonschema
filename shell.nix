with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    pkgconfig
    zlib

    pcre

    # for `git:` references in stack.yml:
    git
  ];
}
