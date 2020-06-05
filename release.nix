{ compiler ? "ghc883" }:

let
  bootstrap = import <nixpkgs> { };

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2d2f04b6a457922627e43e97e3e6e2cde86bc69b";
    sha256 = "1hgf1qx271bdr0jkzs5b965njsk2kk5p2pya02ks90zxfk3ksjms";
  };

  pkgs = import src { };

  haskellPackages = pkgs.haskell.packages."${compiler}";

in haskellPackages.extend
(pkgs.haskell.lib.packageSourceOverrides { ad-mnist = ./.; })
