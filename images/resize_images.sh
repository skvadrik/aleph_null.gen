#!/bin/sh

if [ $# -ne 1 ]
then
    echo "usage: ./resize_images.sh <folder>"
    exit 1
fi

for f in "$1"/*
do
    width=$(identify -ping -format '%w' $f)
    height=$(identify -ping -format '%h' $f)
    if [ $width -gt $height ]; then
      convert -resize 1000x750 -unsharp 0x0.5+1.0+0 -quality 100% $f $f
    else
      convert -resize 750x1000 -unsharp 0x0.5+1.0+0 -quality 100% $f $f
    fi
    # convert -resize 800x600 $f $f
    # convert -resize 720x540 $f $f
done
