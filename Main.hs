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
import qualified Data.List as List

import Control.Monad (forever, zipWithM_, unless, when)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)
import Types (SearchResult (..), Wort (Wort), extractWord, compareWordsCaseInsensitive)

main :: IO ()
main = forever $ do
    op <- pickOperation "===== PICK OPERATION ====="
    case op of
        Validate -> AnkiDB.validateNotes
        Download -> downloadWordsWithoutPron
        UpdateDB -> do
            AnkiDB.addPronReferences
            AnkiDB.moveMp3sToMediaDir
        PlaySounds -> Download.playDownloaded
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
    | PlaySounds
    | UpdateDB
    | Quit

operations :: [(Operation, Text)]
operations =
    [ (Validate, "Validate notes in Anki DB")
    , (Download, "Download pron mp3 files")
    , (PlaySounds, "Play downloaded sounds")
    , (UpdateDB, "Update Anki DB and copy mp3s media folder")
    , (Quit, "Quit")
    ]

-- Download operation
downloadWordsWithoutPron :: IO ()
downloadWordsWithoutPron = do
    wordsToBeDownloaded <- determineWhatNeedsToBeDownloaded
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
    
    updateFilterFiles pronNotAvailable notFound

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

determineWhatNeedsToBeDownloaded :: IO (Set Wort)
determineWhatNeedsToBeDownloaded = do
    wordsWithoutPronInAnkiDb <- fmap (Set.fromList . fmap extractWord) AnkiDB.getWordNotesWithoutPron
    wordsAlreadyDownloaded <- Download.getWordsCorrespondingToDownloadedMp3s
    wordsWithoutPronInDict <- loadWordsFromFile wordsWithoutPronFile
    wordsNotInDict <- loadWordsFromFile wordsNotFoundFile
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
    return wordsToBeDownloaded

loadWordsFromFile :: FilePath -> IO (Set Wort)
loadWordsFromFile = fmap (Set.fromList . fmap (Wort . Text.unpack) . Text.lines) . Text.readFile

updateFilterFiles :: [Wort] -> [Wort] -> IO ()
updateFilterFiles pronNotAvailable notFound =
    when ((length pronNotAvailable + length notFound) > 0) $ do
      putStrLn "Would you like to update filter files? [y/N]"
      resp <- getLine
      when (resp `elem` ["y","Y"]) $ do
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