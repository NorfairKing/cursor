final: prev:
with final.lib;
with final.haskell.lib;

{

  cursorRelease =
    final.symlinkJoin {
      name = "cursor-release";
      paths = attrValues final.haskellPackages.cursorPackages;
    };

  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = composeExtensions (old.overrides or (_: _: { })) (
        self: super:

          let
            cursorPackages =
              let
                cursorPkg = name:
                  buildStrictly (
                    self.callPackage (../${name}) { }
                  );
              in
              final.lib.genAttrs [
                "cursor"
                "cursor-gen"
              ]
                cursorPkg;
          in
          { inherit cursorPackages; } // cursorPackages
      );
    });
}
