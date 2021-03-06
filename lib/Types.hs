{-# LANGUAGE LambdaCase #-}

module Types where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text as Text
import qualified Data.Text.ICU as TI
import Data.Text.Lazy (Text)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Text.Regex (mkRegex, subRegex)


newtype Wort = Wort String deriving (Eq, Ord)


compareWordsCaseInsensitive :: Wort -> Wort -> Ordering
compareWordsCaseInsensitive (Wort a) (Wort b) = TI.compare [TI.CompareIgnoreCase] (Text.pack a) (Text.pack b)


instance Show Wort where
    show (Wort w) = w


newtype Mp3Url = Mp3Url Text deriving (Eq, Show)


data SearchResult
    = PronFound Mp3Url
    | PronNotAvailable
    | NotFound
    | Unknown
    deriving (Eq)


instance Show SearchResult where
    show = \case
        PronFound mp3Url -> show mp3Url
        PronNotAvailable -> "Pron N/A"
        NotFound -> "Not in dictionary"
        Unknown -> "UNEXPECTED ERROR"


data AnkiNote = AnkiNote
    { noteId :: Int
    , noteFlds :: String
    , noteTags :: String
    }
    deriving (Show)


instance FromRow AnkiNote where
    fromRow = AnkiNote <$> field <*> field <*> field


-- Extracting info from anki notes
getFields :: AnkiNote -> [String]
getFields = splitOn "\US" . noteFlds


getCzech :: AnkiNote -> String
getCzech = (!! 0) . getFields


getDeutsch :: AnkiNote -> String
getDeutsch = (!! 1) . getFields


getExamples :: AnkiNote -> String
getExamples = (!! 2) . getFields


getY :: AnkiNote -> String
getY = (!! 3) . getFields


getFieldsWithAddedMp3Reference :: FilePath -> AnkiNote -> String
getFieldsWithAddedMp3Reference mp3File note =
    case getFields note of
        [czech, deutsch, example, y] ->
            let deutschWithMp3Ref = deutsch <> "[sound:" <> mp3File <> "]"
             in intercalate "\US" [czech, deutschWithMp3Ref, example, y]
        fields -> error $ "Expected 4 fields, but got " <> show fields


-- | Primary deutsch word represented by the note (only valid for cards with 'wort' tag)
extractWord :: AnkiNote -> Wort
extractWord = Wort . deleteSpacesAndSlashes . deleteArticles . deletePartAfterDash . deleteSound . deleteThingsInParens . getDeutsch
  where
    deleteSound = delRegex "\\[sound:.*\\.mp3\\]"
    deleteThingsInParens = delRegex "\\([^\\)]*\\)"
    deletePartAfterDash = delRegex " - .*"
    deleteArticles s = let ws = words s in if length ws > 1 then last ws else s
    deleteSpacesAndSlashes = filter (\c -> not (isSpace c) && c /= '/')
    delRegex regex input = subRegex (mkRegex regex) input "" --subst regex by empty String
