# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

This project uses Haskell Stack, but **do not call `stack` directly**. A `ghciwatch` process runs in the background, recompiling on changes and writing errors to `ghciwatch.log`. To check if the project compiles, read the contents of `ghciwatch.log`.

## Formatting

Run `make format` to format code (uses `fourmolu` for .hs files and `cabal-fmt` for .cabal).

## Tests

Tests are integration tests (HSpec) that hit real external dictionary APIs. Run with `stack test`.

## Architecture

Tool for downloading pronunciation MP3s and generating example sentences for Anki flashcard decks (Deutsch, English, Portuguese).

**Core flow:**
- `Main.hs` — CLI entry point with three commands: `DumpWords`, `Download`, `GenExamples`
- `lib/AnkiDB.hs` — SQLite queries against Anki's `collection.anki2` database; deck identification via hardcoded model IDs
- `lib/Search/DWDS.hs`, `lib/Search/Duden.hs`, `lib/Search/VocabularyCom.hs` — HTML scraping of online dictionaries to find MP3 URLs. Deutsch tries DWDS first, falls back to Duden
- `lib/Download.hs` — Downloads MP3s via `wget` to `Download/` directory
- `lib/GenExamples.hs` — Generates example sentences (via `claude` CLI) and word pronunciations (via `edge-tts`), with async prefetching of the next example
- `lib/Types.hs` — Core types: `Wort`, `AnkiNote`, `SearchResult`, `Mp3Url`

**External tools used at runtime:** `wget`, `mplayer`, `edge-tts`, `claude` CLI.
