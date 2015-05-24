MODULE=$1
EXECUTABLE=$2

ghc -o $EXECUTABLE -package-db=./.cabal-sandbox/x86_64-linux-ghc-7.10.1-packages.conf.d/ $MODULE.hs
rm *.hi *.o
