let
  package = import ./release.nix { };
  mnist-data = import ./data;
in (package.shellFor {
  packages = p: [ p.ad-mnist ];
  withHoogle = true;
}).overrideAttrs (_: { MNIST_DATA_DIR = mnist-data; })
