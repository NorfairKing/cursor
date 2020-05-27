let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs' = pkgsv {};
  validity-overlay =
    import (
      (
        pkgs'.fetchFromGitHub (import ./validity-version.nix)
        + "/nix/overlay.nix"
      )
    );
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/a3bd860016653ab53ed49e2c4523e3e7297e58bb.tar.gz"
    );
in
pkgsv {
  overlays =
    [
      validity-overlay
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
}
