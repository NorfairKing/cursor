let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "cursor-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    haskellPackages.autoexporter
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
