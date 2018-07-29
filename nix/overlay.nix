final:
  previous:
    with final.haskell.lib;
    {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super:
            let cursorPkg = name:
                (self.callCabal2nix name (../. + "/${name}") {});
            in final.lib.genAttrs [
              "cursor"
              "cursor-gen"
            ] cursorPkg
        );
      });
    }
