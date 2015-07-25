./build/ghcbuild_module.sh Align align
./build/ghcbuild_module.sh Colocate colocate
./build/ghcbuild_module.sh Columns columns
./build/ghcbuild.sh squeeze squeeze

cd Unzip
./make
cd ..
rm *.hi *.o
