for f in [01]*.jpg; do
    convert -resize 710x533 $f $f
    convert -composite -gravity center background.png $f ${f%.jpg}.png
done
