with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "none";
  buildInputs = [
      pythonPackages.jsonschema
    ];
}
