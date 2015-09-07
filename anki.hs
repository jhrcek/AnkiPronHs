{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Control.Applicative
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Database.SQLite.Simple (Connection, withConnection, query_, execute, Query)
import Database.SQLite.Simple.FromRow
import "regex-compat-tdfa" Text.Regex (subRegex, mkRegex) --Unicode support in regexes
import Text.Regex.Posix ((=~))
import Text.Printf (printf)

data AnkiNote = AnkiNote
    { noteId :: Int
    , noteFlds :: String
    , noteTags :: String 
    }  deriving (Show)

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

-- | Primary deutsch word represented by the note (only valid for cards with 'wort' tag)
extractWord :: AnkiNote -> String
extractWord = deleteSpacesAndSlashes . deleteArticles . deletePartAfterDash . deleteSound . deleteThingsInParens . getDeutsch
  where
     deleteSound = delRegex "\\[sound:.*\\.mp3\\]"
     deleteThingsInParens = delRegex "\\([^\\)]*\\)"
     deletePartAfterDash = delRegex " - .*"
     deleteArticles s = let ws = words s in if length ws > 1 then last ws else s
     deleteSpacesAndSlashes = filter (\c -> not (isSpace c) && c /= '/')
     delRegex regex input = subRegex (mkRegex regex) input "" --subst regex by empty String


allNotes, allWordNotes, notesWithoutPron :: Query
allNotes = "SELECT id,flds,tags FROM notes"
allWordNotes = "SELECT id,flds,tags FROM notes WHERE tags LIKE '%wort%';"
notesWithoutPron = "SELECT id,flds,tags FROM notes WHERE tags LIKE '%wort%' AND flds NOT LIKE '%.mp3%';"


-- Open DB and verify its contents
main :: IO ()
main = withConnection "collection.anki2" $ \conn -> validateNotes conn
--main = withConnection "collection.anki2" $ \conn -> query_ conn allWordNotes >>= \notes -> mapM_ (putStrLn . extractWord) notes

-- Verifying integrity of anki notes
validateNotes :: Connection -> IO ()
validateNotes conn = do
    notes <- query_ conn allNotes :: IO [AnkiNote]
    forM_ noteRules $ \(rule, description) -> do
        let badNotes = filter rule notes
        if null badNotes
            then putStrLn $ "PASSED: " ++ description
            else do 
                putStrLn $ printf "FAILED: %s (%d notes)" description (length badNotes)
                mapM_ (putStrLn . (\n -> printf "   --->  nid:%d %s" (noteId n) (noteFlds n))) badNotes

{-  let badNotes = filter hasQuot notes
    mapM_ (updateNote conn) badNotes  -}

replaceInFlds :: AnkiNote -> String -> String -> (String, Int) 
replaceInFlds note regex replacement = (newFlds, noteId note)
  where newFlds = subRegex (mkRegex regex) (noteFlds note) replacement

updateNote :: Connection -> AnkiNote -> IO()
updateNote con note = do
    let (a,b) = replaceInFlds note "&quot;" "\""
    putStrLn $ "  " ++ noteFlds note ++ "\n->" ++ a
    execute con "update notes set flds = ? where id = ?;" (a,b)

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

wrongFieldCount :: NoteFilter --each note must have 4 fields
wrongFieldCount = (/= 4) . length . getFields 

lastFieldNotY :: NoteFilter --last field of each note must be y
lastFieldNotY = (/= "y") . getY

maskFemNeutWithoutWort :: NoteFilter --Every note which has Maskulinum, Femininum or Neutrum must also have "wort" tag
maskFemNeutWithoutWort note = any (`isInfixOf` tags) ["Maskulinum", "Femininum" , "Neutrum" ]  &&  not ("wort" `isInfixOf` tags)
    where tags = noteTags note

derDieDasWithoutTag :: NoteFilter -- Word has r/e/s <=> it has Maskulinum/Femininum/Neutrum tag
derDieDasWithoutTag note = 
       prefixAndTagInconsistent ["r ", "r/e ", "r/s "] "Maskulinum"
    || prefixAndTagInconsistent ["e ", "r/e "]         "Femininum"
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
hasLeadingOrTrailingWhiteSpaces = fieldMatches "^ +.*|.* +$"

----- Filter helpers -----
containsUndesired :: String -> NoteFilter
containsUndesired str note = str `isInfixOf` noteFlds note

fieldMatches :: String -> NoteFilter
fieldMatches regex note = any (=~ regex) $ getFields note
