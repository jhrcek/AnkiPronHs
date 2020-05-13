# Anki pronunciation downloader

This tool queries my Anki deck for words that don't have associated pronunciation,
searches the words in dictionary, downloads the associated pronunciation mp3 files
and adds reference to the file to anki database.

# Installation (Fedora)

```bash
sudo dnf install libicu-devel
stack install --pedantic --local-bin-path ~/Dropbox/Softy/AnkiPron/
```

