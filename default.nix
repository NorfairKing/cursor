let
  pkgsv = import ( import ./nix/nixpkgs.nix );
  pkgs' = pkgsv {};
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/5c3078ad58856ce22f883b5518879d27bfc59dd5.tar.gz"
    );
  pkgs = import ./nix/pkgs.nix;
in
  pkgs.cursorPackages // {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
     hooks = {
       hindent.enable = true;
       canonix.enable = true;
       hlint.enable = true;
     };
  };
}
