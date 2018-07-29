set -ex

case $BUILD_KIND in
  stack)
    stack setup $RESOLVER_FLAG
    stack build --only-snapshot $RESOLVER_FLAG
  nix)
    echo "Nothing to do for install"
  *)
    echo "Unknown build kind"
esac
