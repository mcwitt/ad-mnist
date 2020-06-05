(import ./release.nix { }).shellFor {
  packages = p: [ p.ad-mnist ];
  withHoogle = true;
}
