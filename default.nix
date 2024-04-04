with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "dev-environment"; 
    buildInputs = [ zlib haskellPackages.calligraphy graphviz ];
}