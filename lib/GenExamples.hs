module GenExamples
    ( genExamples
    , exampleMp3FileName
    ) where

import AnkiDB (Deck (..), getWordNotesWithoutExample, updateNoteFields)
import Data.Char (isSpace, toLower)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)
import Mplayer (playMp3)
import Numeric.Natural (Natural)
import System.IO (hFlush, stdout)
import System.Process (callProcess, readProcess)
import Types (AnkiNote (..), getFieldsWithAddedExample)


genExamples :: Deck -> Maybe Natural -> IO ()
genExamples deck limit = do
    putStrLn $ "Generating examples for deck " <> show deck <> " with limit " <> show limit
    ws <- getWordNotesWithoutExample deck limit
    for_ ws $ \note -> do
        let word =
                -- Assuming the word field looks like "WORD[sound:WORD.mp3]"
                takeWhile (/= '[') (noteLang2 note)
        example <- dropWhileEnd isSpace <$> readProcess "claude" ["--model=sonnet", "-p", promptFor word] ""
        let exampleMp3File = exampleMp3FileName filePrefix example
        putStrLn $ "Note " <> show (noteId note) <> " (" <> word <> "): " <> example <> " -> " <> exampleMp3File
        callProcess
            "edge-tts"
            [ "--voice=" <> voice
            , "--text"
            , example
            , "--write-media"
            , exampleMp3File
            ]
        playMp3 exampleMp3File
        save <- confirm "Save this example to DB?"
        if save
            then do
                let newFlds = getFieldsWithAddedExample example exampleMp3File note
                updateNoteFields note newFlds
                -- TODO automate copying to media dir
                putStrLn "Saved."
            else putStrLn "Skipping"
  where
    (promptFor, voice, filePrefix) = case deck of
        Deutsch ->
            ( \word -> "Generiere einen Beispielsatz auf Deutsch, der das Wort '" <> word <> "' verwendet. Gib nur den Satz aus, nichts anderes."
            , "de-DE-AmalaNeural"
            , "de"
            )
        English ->
            ( \word -> "Generate an example sentence in English using the word '" <> word <> "'. Output only the sentence, nothing else."
            , "en-GB-SoniaNeural"
            , "en"
            )
        Portuguese ->
            ( \word -> "Gere uma frase de exemplo em português brasileiro que use a palavra '" <> word <> "'. Não produza nada além da frase."
            , "pt-BR-FranciscaNeural"
            , "pt"
            )


confirm :: String -> IO Bool
confirm prompt = do
    putStr $ prompt <> " [Y/n] "
    hFlush stdout
    answer <- getLine
    case map toLower answer of
        "" -> pure True
        "y" -> pure True
        "n" -> pure False
        _ -> do
            putStrLn "Please answer y or n."
            confirm prompt


exampleMp3FileName :: String -> String -> FilePath
exampleMp3FileName prefix sentence =
    prefix <> "_" <> sanitize sentence <> ".mp3"
  where
    sanitize = collapseSeparators . dropWhileEnd isSepOrDot . dropWhile isSep
    isSep c = isSpace c || c == ','
    isSepOrDot c = isSep c || c == '.'
    collapseSeparators [] = []
    collapseSeparators (c : cs)
        | isSep c = '_' : collapseSeparators (dropWhile isSep cs)
        | otherwise = c : collapseSeparators cs
