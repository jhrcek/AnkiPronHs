{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import qualified AnkiDB
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Download
import qualified Search.Duden as Duden
import qualified Search.DWDS as DWDS

import Control.Monad (forever, zipWithM_)
import Data.Text (Text)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)
import Types (SearchResult (..), Wort (Wort), extractWord)

main :: IO ()
main = forever $ do
    op <- pickOperation "===== PICK OPERATION ====="
    case op of
        Validate -> AnkiDB.validateNotes
        Download -> downloadWordsWithoutPron
        UpdateDB -> AnkiDB.addPronReferences
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
    , (UpdateDB, "Update Anki DB and copy mp3s media folder")
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
    wordResultPairs <- traverse search $ Set.toList wordsToBeDownloaded
    let toDownload       = [ (wort, mp3Url) | (wort, PronFound mp3Url) <- wordResultPairs ]
        pronNotAvailable = [ wort           | (wort, PronNotAvailable) <- wordResultPairs ]
        notFound         = [ wort           | (wort, NotFound)         <- wordResultPairs ]

    Download.downloadMp3s toDownload

    downloaded <- Set.toList <$> Download.getWordsCorrespondingToDownloadedMp3s

    putStrLn $ unlines
        [ "===== DOWNLOAD SUMMARY ====="
        , "Downloaded         : " <> show downloaded
        , "Pron not available : " <> show pronNotAvailable
        , "Not found          : " <> show notFound
        ]
  where
    loadFromFile :: FilePath -> IO (Set.Set Wort)
    loadFromFile = fmap (Set.fromList . fmap (Wort . Text.unpack) . Text.lines) . Text.readFile

search :: Wort -> IO (Wort, SearchResult)
search wort = do
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
                _           -> return dwdsResult
