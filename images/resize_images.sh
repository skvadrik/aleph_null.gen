#!/bin/sh

if [ $# -ne 1 ]
then
    echo "usage: ./resize_images.sh <folder>"
    exit 1
fi

for f in "$1"/*
do
    # convert -resize 1200x900 $f $f
    convert -resize 1000x750 $f $f
    # convert -resize 800x600 $f $f
    # convert -resize 720x540 $f $f
done
