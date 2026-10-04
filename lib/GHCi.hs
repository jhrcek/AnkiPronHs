module GHCi where

import AnkiDB (Deck (Portuguese))
import GenExamples (ExampleQuery (NotesById), genExamples, textToMp3)


-- | Generate mp3 for Portuguese word/phrase.
p :: String -> IO ()
p =
    textToMp3 Portuguese


-- | Generate portuguese examples for all notes with given Note ID
pex :: [Int] -> IO ()
pex xs =
    genExamples Portuguese $ NotesById xs