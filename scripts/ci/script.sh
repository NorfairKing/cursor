set -ex

case $BUILD_KIND in
  stack)
    stack --no-terminal $RESOLVER_FLAG build --haddock --pedantic
    stack --no-terminal $RESOLVER_FLAG test --pedantic
    ;;
  nix)
    nix-build -A haskellPackages.cursor
    nix-build -A haskellPackages.cursor-gen
    ;;
  *)
    echo "Unknown build kind."
    ;;
esac
