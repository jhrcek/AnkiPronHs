#!/bin/bash

# Download chromedriver binary if it doesn't exist
if [ ! -f chromedriver ]; then
  echo "Downloading latest chromedriver binary"
  VERSION=$(curl http://chromedriver.storage.googleapis.com/LATEST_RELEASE)
  wget --quiet http://chromedriver.storage.googleapis.com/$VERSION/chromedriver_linux64.zip
  unzip chromedriver_linux64.zip
  rm chromedriver_linux64.zip
fi

ANKI_DB=~/Dropbox/Reference/Anki/User\ 1/collection.anki2
echo "Removing old backup"
rm "${ANKI_DB}_bck"
echo "Creating new backup"
cp "$ANKI_DB" "${ANKI_DB}_bck"
echo "Copying collection.anki2 to local dir for further processing"
cp "$ANKI_DB" .
echo "Checking database consistency (Haskell)"
./anki-check
echo "Checking database consistency (Java)"
java -jar ankipron.jar verify

