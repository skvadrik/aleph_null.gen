#!/bin/sh

build_dir=".build"

ghc --make -Wall -O2 -odir $build_dir -hidir $build_dir site.hs
./site build
./site clean
./site watch "$@"
