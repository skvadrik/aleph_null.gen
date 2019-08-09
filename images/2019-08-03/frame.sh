for f in IMG*.jpg; do convert -composite -gravity center background.png $f ${f%.jpg}.png; done
