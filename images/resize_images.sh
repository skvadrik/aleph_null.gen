#!/bin/sh

if [ $# -ne 1 ]
then
    echo "usage: ./resize_images.sh <folder>"
    exit 1
fi

for f in "$1"/*
do
    convert -resize 800x600 $f $f
done
