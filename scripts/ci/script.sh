set -ex

case $BUILD_KIND in
  stack)
    travis_wait 45 stack --no-terminal $RESOLVER_FLAG build --haddock --pedantic $BUILD_PKGS
    travis_wait 45 stack --no-terminal $RESOLVER_FLAG test --pedantic $TEST_PKGS
  nix)
    nix-build -A haskellPackages.cursor
    nix-build -A haskellPackages.cursor-gen
  *)
    echo "Unknown build kind"
esac
