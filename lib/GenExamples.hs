module GenExamples
    ( genExamples
    ) where

import AnkiDB (Deck (..), getWordNotesWithoutExample)
import Data.Foldable (for_)
import Numeric.Natural (Natural)
import Types (AnkiNote (..), getFields)


genExamples :: Deck -> Maybe Natural -> IO ()
genExamples deck@Portuguese limit = do
    putStrLn $ "Generating examples for deck " <> show deck <> " with limit " <> show limit
    ws <- getWordNotesWithoutExample deck limit
    for_ ws $ \note -> do
        putStrLn $ "Generating example for note " <> show (noteId note) <> " with fields " <> show (getFields note)
genExamples deck _ = error $ "genExamples: deck " <> show deck <> " not supported yet"
