import (import ./nix/nixpkgs.nix) {
  overlays = [ (import ./nix/overlay.nix) ];
  config.allowUnfree = true;
}
