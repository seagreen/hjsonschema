with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    pkgconfig
    pcre

    # for `git:` references in stack.yml:
    git
  ];
}
