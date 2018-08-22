module Download
    ( getWordsCorrespondingToDownloadedMp3s
    , downloadMp3s
    ) where

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

import Control.Monad (unless)
import Data.Set (Set, fromList)
import System.Directory (doesDirectoryExist, listDirectory, removeFile)
import System.Exit (die)
import System.FilePath (dropExtension)
import System.Process (callProcess)
import Types (Mp3Url (..), Wort (..))

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

downloadMp3s :: [Mp3Url] -> IO ()
downloadMp3s [] = return ()
downloadMp3s urls = do
    let inputFile = "urls.tmp"
    Text.writeFile inputFile . Text.unlines $ fmap unMp3Url urls
    callProcess "wget"
        [ "--quiet"
        , "--no-check-certificate"
        , "--directory-prefix", downloadDir
        , "--input-file", inputFile
        ]
    removeFile inputFile
