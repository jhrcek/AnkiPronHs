{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AnkiDB
    ( Deck (..)
    , validateNotes
    , getWordNotesWithoutPron
    , addPronReferences
    , moveMp3sToMediaDir
    )
where

import Data.Foldable (for_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query
    , executeMany
    , query
    , setTrace
    , withConnection
    , withExclusiveTransaction
    )
import Database.SQLite.Simple.ToField (ToField, toField)
import Download (downloadDir, getDownloadedMp3FileName, getDownloadedMp3s)
import System.Directory (getHomeDirectory, renamePath)
import System.FilePath ((</>))
import Text.Regex.PCRE.Heavy (Regex, re, (=~))
import Types
    ( AnkiNote (..)
    , getDeutsch
    , getFields
    , getFieldsWithAddedMp3Reference
    , getY
    )


-- Verifying integrity of Anki notes
validateNotes :: Deck -> IO ()
validateNotes deck = withAnkiDB $ \conn -> do
    notes <- query conn allNotes (Only deck) :: IO [AnkiNote]
    for_ noteRules $ \(rule, description) -> case filter rule notes of
        [] ->
            putStrLn $ "PASSED: " <> description
        badNotes -> do
            putStrLn $ "FAILED: " <> description <> " (" <> show (length badNotes) <> " notes)"
            putStrLn . unlines $ fmap show badNotes


type NoteFilter = AnkiNote -> Bool


noteRules :: [(NoteFilter, String)]
noteRules =
    [ (wrongFieldCount, "Note must have 4 fields")
    , (lastFieldNotY, "The last field of note must be 'y'")
    , (maskFemNeutWithoutWort, "Note with Maskulinum/Femininum/Neutrum must have 'wort' tag")
    , (hasNbsp, "Note mustn't contain &nbsp;")
    , (hasQuot, "Note mustn't contain &quot;")
    , (hasSpan, "Note mustn't contain <span")
    , (derDieDasWithoutTag, "Tag Maskulinum/Femininum/Neutrum must be consistent with the article r/e/s")
    , (hasLeadingOrTrailingWhiteSpaces, "Note mustn't have leading/trailing whitespace")
    ]


wrongFieldCount :: NoteFilter -- each note must have 4 fields
wrongFieldCount = (/= 4) . length . getFields


lastFieldNotY :: NoteFilter -- last field of each note must be y
lastFieldNotY = (/= "y") . getY


maskFemNeutWithoutWort :: NoteFilter -- Every note which has Maskulinum, Femininum or Neutrum must also have "wort" tag
maskFemNeutWithoutWort AnkiNote{noteTags} =
    any (`isInfixOf` noteTags) ["Maskulinum", "Femininum", "Neutrum"] && not ("wort" `isInfixOf` noteTags)


derDieDasWithoutTag :: NoteFilter -- Word has r/e/s <=> it has Maskulinum/Femininum/Neutrum tag
derDieDasWithoutTag note =
    prefixAndTagInconsistent ["r ", "r/e ", "r/s "] "Maskulinum"
        || prefixAndTagInconsistent ["e ", "r/e "] "Femininum"
        || prefixAndTagInconsistent ["s ", "(s) ", "r/s "] "Neutrum"
  where
    prefixAndTagInconsistent prefixes tag = any (`isPrefixOf` deutsch) prefixes `xor` (tag `isInfixOf` tags)
    deutsch = getDeutsch note
    tags = noteTags note
    x `xor` y = (x || y) && not (x && y)


hasNbsp, hasQuot, hasSpan :: NoteFilter
hasNbsp = containsUndesired "&nbsp;"
hasQuot = containsUndesired "&quot;"
hasSpan = containsUndesired "<span"


hasLeadingOrTrailingWhiteSpaces :: NoteFilter
hasLeadingOrTrailingWhiteSpaces = fieldMatches [re|^ +.*|.* +$|]


containsUndesired :: String -> NoteFilter
containsUndesired str note = str `isInfixOf` noteFlds note


fieldMatches :: Regex -> NoteFilter
fieldMatches regex note = any (=~ regex) $ getFields note


getWordNotesWithoutPron :: Deck -> IO [AnkiNote]
getWordNotesWithoutPron deck =
    withAnkiDB (\conn -> query conn wordNotesWithoutPron (Only deck))


addPronReferences :: Deck -> IO ()
addPronReferences deck = do
    putStrLn "===== ANKI DB UPDATE ====="
    withAnkiDB $ \conn -> do
        notes <- query conn wordNotesWithoutPron (Only deck)
        -- LOG SQL update query being executed
        setTrace conn (Just Text.putStrLn)
        fldsAndNotes <- for notes $ \note -> do
            maybeMp3File <- getDownloadedMp3FileName note
            pure $
                fmap
                    ( \mp3File ->
                        let newFlds = getFieldsWithAddedMp3Reference mp3File note
                         in (newFlds, noteId note)
                    )
                    maybeMp3File
        executeMany conn addPronQuery (catMaybes fldsAndNotes)


withAnkiDB :: (Connection -> IO a) -> IO a
withAnkiDB action = do
    ankiDB <- getAnkiDbFile
    withConnection ankiDB $ \conn ->
        withExclusiveTransaction conn $ action conn


getAnkiDbFile :: IO FilePath
getAnkiDbFile =
    getAnkiPath "collection.anki2"


moveMp3sToMediaDir :: IO ()
moveMp3sToMediaDir = do
    mp3s <- getDownloadedMp3s
    targetDir <- getAnkiMediaDirectory
    putStrLn $ "Moving " <> show (length mp3s) <> " mp3 files to " <> targetDir
    for_ mp3s $ \mp3 ->
        renamePath (downloadDir </> mp3) (targetDir </> mp3)


getAnkiMediaDirectory :: IO FilePath
getAnkiMediaDirectory =
    getAnkiPath "collection.media"


getAnkiPath :: FilePath -> IO FilePath
getAnkiPath fileName = do
    home <- getHomeDirectory
    pure $ home </> "Dropbox/Reference/Anki/User 1" </> fileName


----- Queries -----
-- notes specify mid (model id) based on which we determine that the note is part of HrkDeutsch
allNotes :: Query
allNotes =
    "SELECT id, flds, tags \
    \FROM notes \
    \WHERE mid = ?"


wordNotesWithoutPron :: Query
wordNotesWithoutPron =
    "SELECT id, flds, tags \
    \FROM notes \
    \WHERE tags LIKE '%wort%' AND flds NOT LIKE '%.mp3%' AND mid = ?"


addPronQuery :: Query
addPronQuery =
    "UPDATE notes \
    \SET flds = ? \
    \WHERE id = ?"


data Deck = HrkDeutsch | HrkEnglish


instance ToField Deck where
    toField = toField . modelId


modelId :: Deck -> Int
modelId = \case
    HrkDeutsch -> 1852153645
    HrkEnglish -> 1723177457855
