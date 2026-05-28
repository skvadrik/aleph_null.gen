#!/bin/sh

if [ $# -ne 1 ]
then
    echo "usage: ./rename_images.sh <folder>"
    exit 1
fi

(
    cd $1
    i=0
    for f in `ls *.jpg | sort` ; do
        mv $f $(printf '%03d.jpg' $i)
        i=$((i + 1))
    done
)
