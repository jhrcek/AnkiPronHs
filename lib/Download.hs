module Download
    ( getWordsCorrespondingToDownloadedMp3s
    , downloadMp3s
    , getDownloadedMp3FileName
    ) where

import qualified Data.Text.Lazy as Text

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Set (Set, fromList)
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
    callProcess "wget"
        [ "--no-verbose"
        , "--no-check-certificate"
        , "--output-document=" <> downloadDir <> "/" <> wort <> ".mp3"
        , Text.unpack url
        ]

getDownloadedMp3FileName :: AnkiNote -> IO (Maybe FilePath)
getDownloadedMp3FileName note = do
    exists <- doesFileExist mp3Path
    if exists
      then return $ Just mp3FileName
      else return Nothing
  where
    (Wort wort) = extractWord note
    mp3FileName = wort <.> "mp3"
    mp3Path = downloadDir </> mp3FileName
