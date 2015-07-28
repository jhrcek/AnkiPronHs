#!/bin/bash

ANKI_DB=~/Dropbox/Reference/Anki/User\ 1/collection.anki2
echo "Removing old backup"
rm "${ANKI_DB}_bck"
echo "Creating new backup"
cp "$ANKI_DB" "${ANKI_DB}_bck"
echo "Copying collection.anki2 to local dir for further processing"
cp "$ANKI_DB" .
echo "Checking database consistency (Haskell)"
runghc anki.hs
echo "Checking database consistency (Java)"
java -jar ankipron.jar verify
