set -ex

source /lib/travis/build/script/templates/header.sh

case $BUILD_KIND in
  stack)
    travis_wait 45 stack --no-terminal $RESOLVER_FLAG build --haddock --pedantic
    travis_wait 45 stack --no-terminal $RESOLVER_FLAG test --pedantic
    ;;
  nix)
    travis_wait 45 nix-build -A haskellPackages.cursor
    travis_wait 45 nix-build -A haskellPackages.cursor-gen
    ;;
  *)
    echo "Unknown build kind."
    ;;
esac
