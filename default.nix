let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs' = pkgsv {};
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/f709c4652d4696dbe7c6a8354ebd5938f2bf807b.tar.gz"
    );
  pkgs = import ./nix/pkgs.nix;
in
pkgs.cursorPackages // {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
