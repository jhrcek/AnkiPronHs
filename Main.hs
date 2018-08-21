{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AnkiDB
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Monad (zipWithM_)
import Data.Text (Text)
import Data.Text.Read (decimal)

main :: IO ()
main = do
    op <- pickOperation "Pick operation:"
    case op of
        Validate -> AnkiDB.validateNotes
        Download -> AnkiDB.getNotesWithoutPron >>= print
        UpdateDB -> return ()

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
    | UpdateDB

operations :: [(Operation, Text)]
operations =
    [ (Validate, "Validate notes in Anki DB")
    , (Download, "Download pron mp3 files")
    , (UpdateDB, "Update Anki DB with downloaded pron files")
    ]
