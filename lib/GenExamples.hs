module GenExamples
    ( genExamples
    , textToMp3
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
        exampleMp3File <- textToMp3 deck example
        putStrLn $ "Note (" <> word <> "): " <> example <> " -> " <> exampleMp3File
        save <- confirm "Save this example to DB?"
        if save
            then do
                let newFlds = getFieldsWithAddedExample example exampleMp3File note
                updateNoteFields note newFlds
                -- TODO automate copying to media dir
                putStrLn "Saved."
            else putStrLn "Skipping"
  where
    promptFor word = case deck of
        Deutsch ->
            "Generiere einen Beispielsatz auf Deutsch, der das Wort '"
                <> word
                <> "' verwendet.\
                   \ Wenn das Wort ein Verb in einer bestimmten Form ist, muss der Satz es in genau dieser Form verwenden.\
                   \ Gib nur den Satz aus, nichts anderes."
        English ->
            "Generate an example sentence in English using the word '"
                <> word
                <> "'.\
                   \ If the word is a verb in a specific form, the sentence must use it in that exact form.\
                   \ Output only the sentence, nothing else."
        Portuguese ->
            "Gere uma frase de exemplo em português brasileiro que use a palavra '"
                <> word
                <> "'.\
                   \ Se a palavra for um verbo numa forma específica, a frase deve usá-la nessa mesma forma.\
                   \ Não produza nada além da frase."


textToMp3 :: Deck -> String -> IO FilePath
textToMp3 deck text = do
    let mp3File = exampleMp3FileName filePrefix text
    callProcess
        "edge-tts"
        [ "--voice=" <> voice
        , "--text"
        , text
        , "--write-media"
        , mp3File
        ]
    playMp3 mp3File
    pure mp3File
  where
    (voice, filePrefix) = case deck of
        Deutsch -> ("de-DE-AmalaNeural", "de")
        English -> ("en-GB-SoniaNeural", "en")
        Portuguese -> ("pt-BR-FranciscaNeural", "pt")


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
