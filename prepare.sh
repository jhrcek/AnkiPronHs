#!/bin/bash
echo "Removing old backup"
rm ~/Dropbox/Reference/Anki/User\ 1/collection.anki2_bck
echo "Creating new backup"
cp ~/Dropbox/Reference/Anki/User\ 1/collection.anki2 ~/Dropbox/Reference/Anki/User\ 1/collection.anki2_bck
echo "Copying collection.anki2 to local dir for further processing"
cp ~/Dropbox/Reference/Anki/User\ 1/collection.anki2 .
echo "Checking database consistency (Haskell)"
runghc anki.hs
echo "Checking database consistency (Java)"
java -jar ankipron.jar verify
