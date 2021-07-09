module Search.Exception (httpExceptionHandler) where

import Control.Lens ((^.))
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq (responseStatus, statusCode)
import Types (SearchResult (..))


httpExceptionHandler :: HttpException -> IO SearchResult
httpExceptionHandler ex = do
    case ex of
        HttpExceptionRequest _request content -> case content of
            ConnectionTimeout -> putStrLn "Request timed out"
            StatusCodeException response _body -> putStrLn $ "Request failed, statusCode = " <> show (response ^. responseStatus . statusCode)
            _ -> putStrLn $ "Other exception: " <> show ex
        InvalidUrlException _ _ -> print ex
    pure NotFound