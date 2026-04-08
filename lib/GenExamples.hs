{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GenExamples
    ( ExampleQuery (..)
    , genExamples
    , genWordPron
    , textToMp3
    ) where

import AnkiDB
    ( Deck (..)
    , getAnkiMediaDirectory
    , getNotesByIds
    , getWordNotesWithoutExample
    , getWordNotesWithoutPron
    , updateNoteFields
    )
import Control.Concurrent.Async (async, wait)
import Data.Char (isDigit, isLetter, isSpace, toLower)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)
import Mplayer (playMp3)
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (callProcess, readProcess)
import Types (AnkiNote (..), Wort (..), extractWord, getFieldsWithAddedExample, getFieldsWithAddedMp3Reference)


data ExampleQuery
    = AllNotes (Maybe Natural) -- optional limit
    | NotesById [Int] -- specific note IDs


-- TODO: modify the getFieldsWithAddedExample to either add or APPEND with <br> the example
-- TODO: add a command to just generate an example mp3 for cards that already have an example, but without pron mp3

genWordPron :: Deck -> IO ()
genWordPron deck = do
    notes <- getWordNotesWithoutPron deck
    putStrLn $ "Found " <> show (length notes) <> " notes without pronunciation"
    for_ notes $ \note -> do
        let Wort word = extractWord note
            mp3FileName = filePrefixForDeck deck <> "_" <> word <> ".mp3"
        mp3FilePath <- generateMp3 deck word mp3FileName
        putStrLn $ word <> " -> [sound:" <> mp3FileName <> "]"
        confirmAndSave mp3FilePath note (getFieldsWithAddedMp3Reference mp3FileName note)


genExamples :: Deck -> ExampleQuery -> IO ()
genExamples deck query = do
    ws <- case query of
        AllNotes limit -> do
            putStrLn $ "Generating examples for deck " <> show deck <> " with limit " <> show limit
            getWordNotesWithoutExample deck limit
        NotesById noteIds -> do
            putStrLn $ "Generating examples for " <> show (length noteIds) <> " specific notes"
            getNotesByIds noteIds
    putStrLn $ "Found " <> show (length ws) <> " notes without example"
    case ws of
        [] -> pure ()
        (first : rest) -> do
            -- Start preparing the first note
            firstPrep <- prepareNote deck first
            -- Process notes with one-step-ahead prefetching
            go firstPrep rest
  where
    go prep [] = presentNote deck prep
    go prep (next : rest) = do
        -- Start preparing next note while user reviews current one
        nextPrepAsync <- async (prepareNote deck next)
        presentNote deck prep
        nextPrep <- wait nextPrepAsync
        go nextPrep rest


-- | Result of preparing a note: LLM example generated and MP3 file created.
data PreparedNote = PreparedNote
    { pNote :: AnkiNote
    , pWord :: String
    , pExample :: String
    , pMp3FileName :: String
    , pMp3FilePath :: FilePath
    }


-- | Call LLM to generate an example and create the MP3 file.
prepareNote :: Deck -> AnkiNote -> IO PreparedNote
prepareNote deck note = do
    let word = takeWhile (/= '[') (noteLang2 note)
    example <- dropWhileEnd isSpace <$> readProcess "claude" ["--model=sonnet", "-p", promptForDeck deck word] ""
    let mp3FileName = exampleMp3FileName (filePrefixForDeck deck) example
    mp3FilePath <- generateMp3 deck example mp3FileName
    pure PreparedNote{pNote = note, pWord = word, pExample = example, pMp3FileName = mp3FileName, pMp3FilePath = mp3FilePath}


-- | Show the prepared example to the user, play audio, and confirm saving.
presentNote :: Deck -> PreparedNote -> IO ()
presentNote _deck PreparedNote{..} = do
    putStr $ "Note '" <> pWord <> "': "
    putStrLn $ pExample <> "[sound:" <> pMp3FileName <> "]"
    confirmAndSave pMp3FilePath pNote (getFieldsWithAddedExample pExample pMp3FileName pNote)


promptForDeck :: Deck -> String -> String
promptForDeck deck word = case deck of
    Deutsch ->
        "Generiere einen Beispielsatz auf Deutsch, der das Wort '"
            <> word
            <> "' verwendet.\
               \ Wenn das Wort ein Verb in einer bestimmten Form ist, muss der Satz es in genau dieser Form verwenden.\
               \ Gib nur den Satz als reinen Text aus, ohne Markdown oder Formatierung."
    English ->
        "Generate an example sentence in English using the word '"
            <> word
            <> "'.\
               \ If the word is a verb in a specific form, the sentence must use it in that exact form.\
               \ Output only the sentence as plain text, no markdown or formatting."
    Portuguese ->
        "Gere uma frase de exemplo em português brasileiro que use a palavra '"
            <> word
            <> "'.\
               \ Se a palavra for um verbo numa forma específica, a frase deve usá-la nessa mesma forma.\
               \ Produza apenas a frase em texto simples, sem markdown ou formatação."


-- | Generate an MP3 file using edge-tts, writing it directly to the Anki media folder.
generateMp3 :: Deck -> String -> FilePath -> IO FilePath
generateMp3 deck text mp3FileName = do
    mediaDir <- getAnkiMediaDirectory
    let mp3FilePath = mediaDir </> mp3FileName
    callProcess
        "edge-tts"
        [ "--voice=" <> voiceForDeck deck
        , "--text"
        , text
        , "--write-media"
        , mp3FilePath
        ]
    pure mp3FilePath


-- | Play the generated MP3, ask user to confirm, and save to DB if confirmed.
confirmAndSave :: FilePath -> AnkiNote -> String -> IO ()
confirmAndSave mp3FilePath note newFlds = do
    playMp3 mp3FilePath
    save <- confirm "Save to DB?"
    if save
        then do
            updateNoteFields note newFlds
            putStrLn "Saved."
        else
            putStrLn "Skipping."


textToMp3 :: Deck -> String -> IO FilePath
textToMp3 deck sentence = do
    let mp3FileName = exampleMp3FileName (filePrefixForDeck deck) sentence
    mp3FilePath <- generateMp3 deck sentence mp3FileName
    putStrLn $ sentence <> "[sound:" <> mp3FileName <> "]"
    playMp3 mp3FilePath
    pure mp3FileName


filePrefixForDeck :: Deck -> String
filePrefixForDeck = \case
    Deutsch -> "de"
    English -> "en"
    Portuguese -> "pt"


exampleMp3FileName :: String -> String -> FilePath
exampleMp3FileName prefix sentence =
    prefix <> "_" <> sanitize sentence <> ".mp3"
  where
    sanitize =
        collapseSeparators
            . dropWhileEnd (not . isLetter)
            . dropWhile (not . isLetter)
            . filter (\c -> isLetter c || isDigit c || isSpace c)
    collapseSeparators [] = []
    collapseSeparators (c : cs)
        | isSpace c = '_' : collapseSeparators (dropWhile isSpace cs)
        | otherwise = c : collapseSeparators cs


voiceForDeck :: Deck -> String
voiceForDeck = \case
    Deutsch -> "de-DE-AmalaNeural"
    English -> "en-GB-SoniaNeural"
    Portuguese -> "pt-BR-FranciscaNeural"


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
