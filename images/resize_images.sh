#!/bin/sh

for f in *
do
    convert -resize 768x512 $f $f
done
