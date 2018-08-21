{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module AnkiDB (validateNotes, getNotesWithoutPron) where

import Data.Foldable (for_)
import Data.List (isInfixOf, isPrefixOf)
import Database.SQLite.Simple (Query, query_, withConnection)
import Text.Regex.TDFA ((=~))
import Types (AnkiNote (..), getDeutsch, getFields, getY)

-- Verifying integrity of anki notes
validateNotes :: IO ()
validateNotes  = withConnection "collection.anki2" $ \conn -> do
    notes <- query_ conn allNotes :: IO [AnkiNote]
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

wrongFieldCount :: NoteFilter --each note must have 4 fields
wrongFieldCount = (/= 4) . length . getFields

lastFieldNotY :: NoteFilter --last field of each note must be y
lastFieldNotY = (/= "y") . getY

maskFemNeutWithoutWort :: NoteFilter --Every note which has Maskulinum, Femininum or Neutrum must also have "wort" tag
maskFemNeutWithoutWort AnkiNote{noteTags} =
    any (`isInfixOf` noteTags) ["Maskulinum", "Femininum" , "Neutrum" ] &&  not ("wort" `isInfixOf` noteTags)


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

getNotesWithoutPron :: IO [AnkiNote]
getNotesWithoutPron =
    withConnection "collection.anki2" $ \conn -> do
        query_ conn notesWithoutPron

----- Queries -----
allNotes :: Query
allNotes = "SELECT id,flds,tags FROM notes"

notesWithoutPron :: Query
notesWithoutPron = "SELECT id,flds,tags FROM notes WHERE tags LIKE '%wort%' AND flds NOT LIKE '%.mp3%';"
