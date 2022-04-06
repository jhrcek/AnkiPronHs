module Download (
    getWordsCorrespondingToDownloadedMp3s,
    downloadMp3s,
    downloadDir,
    getDownloadedMp3FileName,
    getDownloadedMp3s,
    playDownloaded,
) where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Set (Set, fromList)
import qualified Data.Text.Lazy as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath (dropExtension, (<.>), (</>))
import System.Process (callProcess)
import Types (AnkiNote (..), Mp3Url (..), Wort (..), extractWord)


downloadDir :: FilePath
downloadDir = "Download"


getDownloadedMp3s :: IO [FilePath]
getDownloadedMp3s = do
    exists <- doesDirectoryExist downloadDir
    unless exists . die $ "The directory '" <> downloadDir <> "' with mp3 files does not exist"
    listDirectory downloadDir


getWordsCorrespondingToDownloadedMp3s :: IO (Set Wort)
getWordsCorrespondingToDownloadedMp3s = do
    mp3Files <- getDownloadedMp3s
    return . fromList $ fmap (Wort . dropExtension) mp3Files


downloadMp3s :: [(Wort, Mp3Url)] -> IO ()
downloadMp3s xs = do
    putStrLn "===== DOWNLOAD ====="
    traverse_ downloadMp3 xs


downloadMp3 :: (Wort, Mp3Url) -> IO ()
downloadMp3 (Wort wort, Mp3Url url) =
    callProcess
        "wget"
        [ "--no-verbose"
        , "--output-document=" <> downloadDir <> "/" <> wort <> ".mp3"
        , Text.unpack url
        ]
        `catch` logIOException
  where
    logIOException :: IOException -> IO ()
    logIOException _ =
        putStrLn $ "Failed to download pronunciation for " <> wort <> " from " <> Text.unpack url


playDownloaded :: IO ()
playDownloaded = do
    mp3filenames <- getDownloadedMp3s
    let mp3s = fmap (downloadDir </>) mp3filenames
    if null mp3s
        then putStrLn "There are no files to play back"
        else callProcess "mplayer" mp3s


getDownloadedMp3FileName :: AnkiNote -> IO (Maybe FilePath)
getDownloadedMp3FileName note = do
    exists <- doesFileExist mp3Path
    pure $
        if exists
            then Just mp3FileName
            else Nothing
  where
    (Wort wort) = extractWord note
    mp3FileName = wort <.> "mp3"
    mp3Path = downloadDir </> mp3FileName
