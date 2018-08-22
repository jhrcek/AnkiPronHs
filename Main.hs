{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AnkiDB
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Monad (forever, zipWithM_)
import Data.Text (Text)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)
import Types (Wort (Wort), extractWord)

main :: IO ()
main = forever $ do
    op <- pickOperation "Pick operation:"
    case op of
        Validate -> AnkiDB.validateNotes
        Download -> downloaWortsWithoutPron
        UpdateDB -> return ()
        Quit     -> exitSuccess

pickOperation :: Text -> IO Operation
pickOperation prompt = do
    Text.putStrLn prompt
    zipWithM_ showOperation [0::Int ..] operations
    x <- Text.getLine
    case decimal x of
        Right (n, "") | 0 <= n && n < length operations -> return . fst $ operations !! n
        _ -> do
            Text.putStrLn $ "Invalid input " <> x
            pickOperation prompt
  where
    showOperation index (_, description) =
        Text.putStrLn $ mconcat [Text.pack (show index), ") ", description]

data Operation
    = Validate
    | Download
    | UpdateDB
    | Quit

operations :: [(Operation, Text)]
operations =
    [ (Validate, "Validate notes in Anki DB")
    , (Download, "Download pron mp3 files")
    , (UpdateDB, "Update Anki DB with downloaded pron files")
    , (Quit, "Quit")
    ]

-- Download operation
downloaWortsWithoutPron :: IO ()
downloaWortsWithoutPron = do
    wordsWithoutPron <- fmap (Set.fromList . fmap extractWord) AnkiDB.getNotesWithoutPron
    let alreadyDownloaded = Set.empty -- TODO
    pronNotAvailable <- loadFromFile "pron_not_available.txt"
    wordsNotFound <- loadFromFile "words_not_found.txt"
    let wordsToBeDownloaded = foldl1 Set.difference [wordsWithoutPron, pronNotAvailable, alreadyDownloaded, wordsNotFound]
    putStrLn $ unlines
        [ "Word count"
        , "    - without pron in Anki DB: " <> show (length wordsWithoutPron)
        , "    - pron N/A:                " <> show (length pronNotAvailable)
        , "    - not found                " <> show (length wordsNotFound)
        , "    - to be downloaded         " <> show (length wordsToBeDownloaded)
        ]
    mapM_ print wordsToBeDownloaded
  where
    loadFromFile :: FilePath -> IO (Set.Set Wort)
    loadFromFile = fmap (Set.fromList . fmap (Wort . Text.unpack) . Text.lines) . Text.readFile
