final: previous:
with final.haskell.lib;

{


  cursorPackages =
    let
      cursorPkg =
        name:
        (
          buildStrictly (
            final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
          )
        );
    in
    final.lib.genAttrs [
      "cursor"
      "cursor-gen"
    ]
      cursorPkg;
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super: final.cursorPackages
          );
      }
    );
}
