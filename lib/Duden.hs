{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Duden (getMp3Url) where

import qualified Network.Wreq as Wreq

import Control.Exception (handle)
import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client (HttpException)
import Text.HTML.TagSoup (Tag, fromAttrib, parseTags)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)
import Types (Wort (..), Mp3Url (..), SearchResult (..))

getMp3Url :: Wort -> IO SearchResult
getMp3Url (Wort word) = handle handler $ do
    resp <- Wreq.get ("https://www.duden.de/rechtschreibung/" <> (replaceUmlauts word))
    let bodyLBS = resp ^. Wreq.responseBody
    return . extractSearchResult . parseTags $ decodeUtf8 bodyLBS
  where
    handler (_exception :: HttpException) = return NotFound

-- Duden is replacing
replaceUmlauts :: String -> String
replaceUmlauts = concatMap (\case
    'Ä' -> "Ae"
    'ä' -> "ae"
    'Ö' -> "Oe"
    'ö' -> "oe"
    'Ü' -> "Ue"
    'ü' -> "ue"
    c   -> [c])

extractSearchResult :: [Tag Text] -> SearchResult
extractSearchResult tags
    | (mp3Url:_) <- extractMp3Url tags = PronFound mp3Url
    | isWordWithoutPron tags           = PronNotAvailable
    | otherwise                        = Unknown

extractMp3Url :: [Tag Text] -> [Mp3Url]
extractMp3Url =
    fmap (Mp3Url . fromAttrib "href") . filter (tagOpenAttrLit "a" ("class", "audio"))

isWordWithoutPron :: [Tag Text] -> Bool
isWordWithoutPron =
    not . null . filter (tagOpenAttrLit "div" ("class", "entry"))
