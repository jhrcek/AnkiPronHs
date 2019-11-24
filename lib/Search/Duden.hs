{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Search.Duden (search) where

import qualified Network.Wreq as Wreq

import Control.Exception (handle)
import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq (responseStatus, statusCode)
import Text.HTML.TagSoup (Tag, fromAttrib, parseTags)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)
import Types (Mp3Url (..), SearchResult (..), Wort (..))

search :: Wort -> IO SearchResult
search (Wort word) = handle handler $ do
    resp <- Wreq.get ("https://www.duden.de/rechtschreibung/" <> replaceGermanChars word)
    let bodyLBS = resp ^. Wreq.responseBody
    return . extractSearchResult . parseTags $ decodeUtf8 bodyLBS
  where
    handler :: HttpException -> IO SearchResult
    handler ex = do
      case ex of
        HttpExceptionRequest _request content -> case content of
          ConnectionTimeout -> putStrLn "Request timed out"
          StatusCodeException response _body -> putStrLn $ "Request failed, statusCode = " <> show (response ^. responseStatus . statusCode)
          _ -> putStrLn $ "Other exception: " <> show ex
        InvalidUrlException _ _ -> print ex
      pure NotFound


-- Duden is replacing
replaceGermanChars :: String -> String
replaceGermanChars = concatMap (\case
    'Ä' -> "Ae"
    'ä' -> "ae"
    'Ö' -> "Oe"
    'ö' -> "oe"
    'Ü' -> "Ue"
    'ü' -> "ue"
    'ß' -> "sz"
    c   -> [c])

extractSearchResult :: [Tag Text] -> SearchResult
extractSearchResult tags
    | (mp3Url:_) <- extractMp3Url tags = PronFound mp3Url
    | isWordWithoutPron tags           = PronNotAvailable
    | otherwise                        = Unknown

extractMp3Url :: [Tag Text] -> [Mp3Url]
extractMp3Url =
    fmap (Mp3Url . fromAttrib "href") . filter (tagOpenAttrLit "a" ("class", "pronunciation-guide__sound"))

isWordWithoutPron :: [Tag Text] -> Bool
isWordWithoutPron =
    any (tagOpenAttrLit "div" ("class", "lemma"))
