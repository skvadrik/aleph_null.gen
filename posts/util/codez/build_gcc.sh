#!/bin/sh -e

if [ $# -ne 1 ]
then
    echo "usage: ./build_gcc.sh <sourcedir>"
    exit
fi

sourcedir=$1
builddir=$sourcedir-build
installdir=$sourcedir-install

mkdir "$builddir"
mkdir "$installdir"

cd "$builddir"
../"$sourcedir"/configure \
    --prefix="$(pwd)"/../"$installdir" \
    --disable-bootstrap \
    --disable-multilib \
    --disable-multiarch \
    --enable-languages=c++ #,c,...
make -j9
make install
