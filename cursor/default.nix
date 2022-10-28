{ mkDerivation, base, containers, deepseq, lib, microlens, text
, validity, validity-containers, validity-text
}:
mkDerivation {
  pname = "cursor";
  version = "0.3.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq microlens text validity validity-containers
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/cursor";
  description = "Purely Functional Cursors";
  license = lib.licenses.mit;
}
