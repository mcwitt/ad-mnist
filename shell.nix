let
  packages = import ./packages.nix { };
  mnist-data = import ./data;
in (packages.shellFor {
  packages = p: [ p.ad-mnist ];
  withHoogle = true;
  buildInputs = with packages; [ cabal-install ghcide hlint ormolu ];
}).overrideAttrs (_: { MNIST_DATA_DIR = mnist-data; })
