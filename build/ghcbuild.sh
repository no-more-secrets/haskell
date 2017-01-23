MODULE=$1
EXECUTABLE=$2

ghc -dynamic -o $EXECUTABLE -package-db=./.cabal-sandbox/x86_64-linux-ghc-7.10.1-packages.conf.d/ $MODULE.hs
strip $EXECUTABLE
