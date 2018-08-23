module Types where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Text.Lazy (Text)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Text.Regex (mkRegex, subRegex)

newtype Wort = Wort String deriving (Eq, Ord, Show)
newtype Mp3Url = Mp3Url Text deriving (Eq, Show)

data SearchResult
    = PronFound Mp3Url
    | PronNotAvailable
    | NotFound
    | Unknown
    deriving (Eq, Show)

data AnkiNote = AnkiNote
    { noteId   :: Int
    , noteFlds :: String
    , noteTags :: String
    } deriving (Show)

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
    intercalate "\US" [czech, deutschWithMp3Ref, example, y]
  where
    [czech, deutsch, example, y] = getFields note
    deutschWithMp3Ref = deutsch <> "[sound:" <> mp3File <> "]"

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
