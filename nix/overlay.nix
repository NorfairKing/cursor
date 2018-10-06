final:
  previous:
    with final.haskell.lib;
    {
      cursorPackages = 
            let cursorPkg = name:
                (failOnAllWarnings (final.haskellPackages.callCabal2nix name (../. + "/${name}") {}));
            in final.lib.genAttrs [
              "cursor"
              "cursor-gen"
            ] cursorPkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.cursorPackages
        );
      });
    }
