{
  description = "cursor";
  nixConfig = {
    extra-substituters = "https://cursor.cachix.org";
    extra-trusted-public-keys = "cursor.cachix.org-1:1mqR0v1xbBZm08uXByCpaCm/zom3/HZkP4NXevS+kv8=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    nixpkgs-21_05.url = "github:NixOS/nixpkgs?ref=nixos-21.05";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , nixpkgs-21_11
    , nixpkgs-21_05
    , flake-utils
    , pre-commit-hooks
    , validity
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.${system}
            (import (validity + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;
      in
      {
        overlays = import ./nix/overlay.nix;
        packages.default = pkgs.cursorRelease;
        checks =
          let
            backwardCompatibilityCheckFor = nixpkgs:
              let pkgs' = pkgsFor nixpkgs;
              in pkgs'.cursorRelease;
            allNixpkgs = {
              inherit
                nixpkgs-22_05
                nixpkgs-21_11
                nixpkgs-21_05;
            };
            backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
          in
          backwardCompatibilityChecks // {
            pre-commit = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                hpack.enable = true;
                ormolu.enable = true;
                nixpkgs-fmt.enable = true;
                nixpkgs-fmt.excludes = [ ".*/default.nix" ];
                cabal2nix.enable = true;
              };
            };
          };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "cursor-shell";
          packages = p: (builtins.attrValues p.cursorPackages);
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
      });
}
