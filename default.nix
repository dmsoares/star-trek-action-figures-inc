with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "dev-environment"; 
    buildInputs =
        [ zlib
          haskellPackages.haskell-language-server
          haskellPackages.calligraphy
          graphviz
        ];
}
