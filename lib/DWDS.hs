{-# LANGUAGE OverloadedStrings #-}
module DWDS (getMp3Url) where

import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.Wreq as Wreq
import Text.HTML.TagSoup (Tag, fromAttrib, isTagOpenName, parseTags)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)
import Types (DWord (..), Mp3Url (..), SearchResult (..))

getMp3Url :: DWord -> IO SearchResult
getMp3Url (DWord word) = do
    resp <- Wreq.get $ "https://www.dwds.de/wb/" <> word
    let bodyLBS = resp ^. Wreq.responseBody
    return . extractSearchResult . parseTags $ decodeUtf8 bodyLBS

extractSearchResult :: [Tag Text] -> SearchResult
extractSearchResult tags
    | (mp3Url:_) <- extractMp3Url tags = PronFound mp3Url
    | isWordWithoutPron tags           = PronNotAvailable
    | isNotFoundResult tags            = NotFound
    | otherwise                        = Unknown

extractMp3Url :: [Tag Text] -> [Mp3Url]
extractMp3Url =
    -- src attribute contains things like "//media.dwds.de/dwds2/audio/002/der_Hund.mp3"
    fmap (Mp3Url . ("http:" <>) . fromAttrib "src") . filter (isTagOpenName "source")

isWordWithoutPron :: [Tag Text] -> Bool
isWordWithoutPron =
    not . null . filter (tagOpenAttrLit "div" ("class", "dwdswb-artikel"))

isNotFoundResult :: [Tag Text] -> Bool
isNotFoundResult =
    not . null . filter (tagOpenAttrLit "p" ("class", "bg-danger"))