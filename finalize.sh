#!/bin/bash
echo "Adding pron references to collection.anki2"
java -jar ankipron.jar add
echo "Copying collection.anki2 to dropbox"
cp collection.anki2 ~/Dropbox/Reference/Anki/User\ 1/
echo "Copying mp3s to dropbox"
mv Downloaded/* ~/Dropbox/Reference/Anki/User\ 1/collection.media/
rm urls.txt

