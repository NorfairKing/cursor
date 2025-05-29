{
  description = "cursor";
  nixConfig = {
    extra-substituters = "https://cursor.cachix.org";
    extra-trusted-public-keys = "cursor.cachix.org-1:1mqR0v1xbBZm08uXByCpaCm/zom3/HZkP4NXevS+kv8=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    nixpkgs-24_11.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_05.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-24_11
    , nixpkgs-24_05
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.cursorRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.cursorRelease;
          allNixpkgs = {
            inherit
              nixpkgs-24_11
              nixpkgs-24_05;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          release = self.packages.${system}.default;
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
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "cursor-shell";
        packages = p: (builtins.attrValues p.cursorPackages);
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci.cachix = {
        name = "cursor";
        public-key = "cursor.cachix.org-1:1mqR0v1xbBZm08uXByCpaCm/zom3/HZkP4NXevS+kv8=";
      };
    };
}
