{ sources ? import ./nix/sources.nix, compiler ? "ghc883" }:
with import sources.nixpkgs { };
haskell.packages."${compiler}".extend
(haskell.lib.packageSourceOverrides { ad-mnist = nix-gitignore.gitignoreSource [] ./.; })
