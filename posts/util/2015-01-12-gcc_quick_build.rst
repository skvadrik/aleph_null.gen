---
title: quickly build GCC from source
---

-----------------------------
quickly build GCC from source
-----------------------------

To build a minimal compiler (takes about 5 min on Intel Core i7):

.. code-block:: bash

    #!/bin/sh -e

    if [ $# -ne 1 ]
    then
        echo "usage: ./build_gcc.sh <sourcedir>"
        exit
    fi

    sourcedir="$1"
    builddir=$sourcedir-build
    installdir=$sourcedir-install

    mkdir $builddir
    mkdir $installdir

    cd $builddir
    ../$sourcedir/configure \
        --prefix=$(pwd)/../$installdir \
        --disable-bootstrap \
        --disable-multilib \
        --disable-multiarch \
        --enable-languages=c++ #,c, ...
    make -j9
    make install

Details `here <https://gcc.gnu.org/install/>`_.
