{-# LANGUAGE OverloadedStrings #-}

module Search.VocabularyCom (search) where

import Control.Exception (handle)
import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wreq qualified as Wreq
import Search.Exception (httpExceptionHandler)
import Text.HTML.TagSoup (Tag, fromAttrib, parseTags)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)
import Types (Mp3Url (..), SearchResult (..), Wort (..))


search :: Wort -> IO SearchResult
search (Wort word) = handle httpExceptionHandler $ do
    resp <- Wreq.get $ "https://www.vocabulary.com/dictionary/" <> word
    let bodyLBS = resp ^. Wreq.responseBody
    return . extractSearchResult . parseTags $ decodeUtf8 bodyLBS


extractSearchResult :: [Tag Text] -> SearchResult
extractSearchResult tags
    | (mp3Url : _) <- extractMp3Url tags = PronFound mp3Url
    -- \| isWordWithoutPron tags = PronNotAvailable
    | isNotFoundResult tags = NotFound
    | otherwise = Unknown


extractMp3Url :: [Tag Text] -> [Mp3Url]
extractMp3Url =
    -- <a data-audio="C/TWO3DNXS25SP" class="audio"></a>, data-audio points to mp3 url like
    -- https://audio.vocabulary.com/1.0/us/C/TWO3DNXS25SP.mp3
    fmap (Mp3Url . (\x -> "https://audio.vocabulary.com/1.0/us/" <> x <> ".mp3") . fromAttrib "data-audio")
        . filter (tagOpenAttrLit "a" ("class", "audio"))


-- isWordWithoutPron :: [Tag Text] -> Bool
-- isWordWithoutPron =
--     any (tagOpenAttrLit "div" ("class", "dwdswb-artikel"))

isNotFoundResult :: [Tag Text] -> Bool
isNotFoundResult =
    any (tagOpenAttrLit "div" ("class", "wordnotfound-wrapper"))
