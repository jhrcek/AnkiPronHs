{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AnkiDB (Deck (..))
import AnkiDB qualified
import Control.Monad (forever, unless, when, zipWithM_)
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Read (decimal)
import Download qualified
import Options.Generic (Generic, ParseRecord, Text, getRecord)
import Search.DWDS qualified as DWDS
import Search.Duden qualified as Duden
import Search.VocabularyCom qualified as VocabularyCom
import System.Exit (exitSuccess)
import Types (SearchResult (..), Wort (Wort), compareWordsCaseInsensitive, extractWord)


main :: IO ()
main = do
    cmd <- getRecord "Anki Pron Downloader"
    case cmd of
        DumpWords deck -> AnkiDB.dumpAllWords deck
        Download deck -> forever $ do
            op <- pickOperation
            case op of
                Validate -> AnkiDB.validateNotes deck
                DownloadMp3s -> downloadWordsWithoutPron deck
                UpdateDB -> do
                    AnkiDB.addPronReferences deck
                    AnkiDB.moveMp3sToMediaDir
                PlaySounds -> Download.playDownloaded
                Quit -> exitSuccess


data CliCommand
    = DumpWords Deck
    | Download Deck
    deriving stock (Generic, Show)
    deriving anyclass (ParseRecord)


pickOperation :: IO Operation
pickOperation = do
    Text.putStrLn "===== PICK OPERATION ====="
    zipWithM_ showOperation [0 :: Int ..] operations
    x <- Text.getLine
    case decimal x of
        Right (n, "") | 0 <= n && n < length operations -> return . fst $ operations !! n
        _ -> do
            Text.putStrLn $ "Invalid input " <> x
            pickOperation
  where
    showOperation index (_, description) =
        Text.putStrLn $ mconcat [Text.pack (show index), ") ", description]


data Operation
    = Validate
    | DownloadMp3s
    | PlaySounds
    | UpdateDB
    | Quit


operations :: [(Operation, Text)]
operations =
    [ (Validate, "Validate notes in Anki DB")
    , (DownloadMp3s, "Download pron mp3 files")
    , (PlaySounds, "Play downloaded sounds")
    , (UpdateDB, "Update Anki DB and copy mp3s media folder")
    , (Quit, "Quit")
    ]


-- Download operation
downloadWordsWithoutPron :: AnkiDB.Deck -> IO ()
downloadWordsWithoutPron deck = do
    wordsToBeDownloaded <- determineWhatNeedsToBeDownloaded deck
    wordResultPairs <- traverse (search deck) $ Set.toList wordsToBeDownloaded
    let toDownload = [(wort, mp3Url) | (wort, PronFound mp3Url) <- wordResultPairs]
        pronNotAvailable = [wort | (wort, PronNotAvailable) <- wordResultPairs]
        notFound = [wort | (wort, NotFound) <- wordResultPairs]

    Download.downloadMp3s toDownload

    downloaded <- Set.toList <$> Download.getWordsCorrespondingToDownloadedMp3s

    putStrLn $
        unlines
            [ "===== DOWNLOAD SUMMARY ====="
            , "Downloaded         : " <> show downloaded
            , "Pron not available : " <> show pronNotAvailable
            , "Not found          : " <> show notFound
            ]

    updateFilterFiles pronNotAvailable notFound


search :: Deck -> Wort -> IO (Wort, SearchResult)
search = \case
    Deutsch -> deutschSearch
    English -> englishSearch


deutschSearch :: Wort -> IO (Wort, SearchResult)
deutschSearch wort = do
    putStrLn $ "Search " <> show wort
    dwdsResult <- DWDS.search wort
    putStrLn $ "  DWDS: " <> show dwdsResult
    (wort,) <$> case dwdsResult of
        PronFound _ -> return dwdsResult
        _ -> do
            dudenResult <- Duden.search wort
            putStrLn $ "  Duden: " <> show dudenResult
            case dudenResult of
                PronFound _ -> return dudenResult
                _ -> return dwdsResult


englishSearch :: Wort -> IO (Wort, SearchResult)
englishSearch wort = do
    putStrLn $ "Search " <> show wort
    result <- VocabularyCom.search wort
    putStrLn $ "  vocabulary.com: " <> show result
    return (wort, result)


determineWhatNeedsToBeDownloaded :: AnkiDB.Deck -> IO (Set Wort)
determineWhatNeedsToBeDownloaded deck = do
    wordsWithoutPronInAnkiDb <- fmap (Set.fromList . fmap extractWord) (AnkiDB.getWordNotesWithoutPron deck)
    wordsAlreadyDownloaded <- Download.getWordsCorrespondingToDownloadedMp3s
    wordsWithoutPronInDict <- loadWordsFromFile wordsWithoutPronFile
    wordsNotInDict <- loadWordsFromFile wordsNotFoundFile
    let wordsToIgnore = Set.unions [wordsAlreadyDownloaded, wordsWithoutPronInDict, wordsNotInDict]
        wordsToBeDownloaded = Set.difference wordsWithoutPronInAnkiDb wordsToIgnore
    putStrLn $
        unlines
            [ "Word count"
            , "  - without pronunciation in Anki DB : " <> show (length wordsWithoutPronInAnkiDb)
            , "  - already downloaded               : " <> show (length wordsAlreadyDownloaded)
            , "  - pronunciation N/A                : " <> show (length wordsWithoutPronInDict)
            , "  - not found in dictionaries        : " <> show (length wordsNotInDict)
            , "  - to be downloaded                 : " <> show (length wordsToBeDownloaded)
            , "===== SEARCH ====="
            ]
    return wordsToBeDownloaded


loadWordsFromFile :: FilePath -> IO (Set Wort)
loadWordsFromFile = fmap (Set.fromList . fmap (Wort . Text.unpack) . Text.lines) . Text.readFile


updateFilterFiles :: [Wort] -> [Wort] -> IO ()
updateFilterFiles pronNotAvailable notFound =
    when ((length pronNotAvailable + length notFound) > 0) $ do
        putStrLn "Would you like to update filter files? [y/N]"
        resp <- getLine
        when (resp `elem` ["y", "Y"]) $ do
            updateWordsFile wordsWithoutPronFile pronNotAvailable
            updateWordsFile wordsNotFoundFile notFound


updateWordsFile :: FilePath -> [Wort] -> IO ()
updateWordsFile f wordsToAdd =
    unless (null wordsToAdd) $ do
        ws <- loadWordsFromFile f
        let newWords = fmap show . List.sortBy compareWordsCaseInsensitive . Set.toList $ Set.union ws $ Set.fromList wordsToAdd
        writeFile f (unlines newWords)
        putStrLn $ "Added " <> show (length wordsToAdd) <> " words to a file " <> f


wordsWithoutPronFile :: FilePath
wordsWithoutPronFile = "words_without_pron_in_dict"


wordsNotFoundFile :: FilePath
wordsNotFoundFile = "words_not_in_dict"
