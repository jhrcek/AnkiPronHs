#!/bin/bash
ANKI_DB=~/Dropbox/Reference/Anki/User\ 1/collection.anki2
echo "Creating backup copy of Anki DB"
cp "$ANKI_DB" "${ANKI_DB}_bck"
echo "Copying collection.anki2 to local dir for further processing"
cp "$ANKI_DB" .
stack exec anki-exe
