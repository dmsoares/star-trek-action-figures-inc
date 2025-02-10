with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "dev-environment";
  buildInputs = [ # haskellPackages.haskell-language-server
    haskellPackages.calligraphy
    graphviz
  ];
}
