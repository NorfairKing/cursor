let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs' = pkgsv {};
  validity-overlay = import (
    (pkgs'.fetchFromGitHub (import ./nix/validity-version.nix)
    + "/nix/overlay.nix")
  );
  pkgs = pkgsv {
    overlays = [ validity-overlay (import ./nix/overlay.nix) ];
    config.allowUnfree = true;
  };
in pkgs.cursorPackages
