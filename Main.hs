{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AnkiDB
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Download
import qualified DWDS

import Control.Monad (forever, zipWithM_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Types (Mp3Url, SearchResult (..), Wort (Wort), extractWord)

main :: IO ()
main = forever $ do
    op <- pickOperation "===== PICK OPERATION ====="
    case op of
        Validate -> AnkiDB.validateNotes
        Download -> downloadWordsWithoutPron
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
downloadWordsWithoutPron :: IO ()
downloadWordsWithoutPron = do
    wordsWithoutPronInAnkiDb <- fmap (Set.fromList . fmap extractWord) AnkiDB.getWordNotesWithoutPron
    wordsAlreadyDownloaded <- Download.getWordsCorrespondingToDownloadedMp3s
    wordsWithoutPronInDict <- loadFromFile "words_without_pron_in_dict"
    wordsNotInDict <- loadFromFile "words_not_in_dict"
    let wordsToIgnore = Set.unions [wordsAlreadyDownloaded, wordsWithoutPronInDict, wordsNotInDict]
        wordsToBeDownloaded = Set.difference wordsWithoutPronInAnkiDb wordsToIgnore
    putStrLn $ unlines
        [ "Word count"
        , "  - without pronunciation in Anki DB : " <> show (length wordsWithoutPronInAnkiDb)
        , "  - already downloaded               : " <> show (length wordsAlreadyDownloaded)
        , "  - pronunciation N/A                : " <> show (length wordsWithoutPronInDict)
        , "  - not found in dictionaries        : " <> show (length wordsNotInDict)
        , "  - to be downloaded                 : " <> show (length wordsToBeDownloaded)
        ,  "===== SEARCH ====="
        ]
    wordMp3Pairs <- fmap catMaybes . traverse search $ Set.toList wordsToBeDownloaded
    Download.downloadMp3s wordMp3Pairs
  where
    loadFromFile :: FilePath -> IO (Set.Set Wort)
    loadFromFile = fmap (Set.fromList . fmap (Wort . Text.unpack) . Text.lines) . Text.readFile

search :: Wort -> IO (Maybe (Wort, Mp3Url))
search wort@(Wort w) = do
    searchResult <- DWDS.search wort
    putStr $ printf "%-16s: " w
    case searchResult of
        PronFound mp3Url -> do
            putStrLn $ "found " <> show mp3Url
            return $ Just (wort, mp3Url)
        PronNotAvailable -> do
            putStrLn "pron N/A"
            return Nothing
        NotFound -> do
            putStrLn "not in dictionary"
            return Nothing
        Unknown -> do
            putStrLn "UNEXPECTED ERROR"
            return Nothing
