{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Server
  ( Application
  , Response
  , Body(..)
  , ContentType
  , listen
  , onTerminate
  , listenWithShutdown
  , getRemainingBody
  , ok
  , created
  , accepted
  , badRequest
  , forbidden
  , notFound
  , conflict
  , methodNotAllowed
  , notImplemented
  , internalServerError
  , textPlain
  , textCsv
  , textHtml
  , textXml
  , applicationJson
  , applicationOctetStream
  , queryParams
  , urlEncode
  , module Data.ByteString
  , module Network.HTTP.Types.Header
  , module Network.HTTP.Types.Method
  , module Network.Wai
  ) where

import qualified Data.Binary.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.String (IsString(..))
import Log
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.HTTP.Types.Status (Status(..), accepted202, badRequest400, conflict409, created201, forbidden403, internalServerError500, methodNotAllowed405, notFound404, notImplemented501, ok200, statusIsSuccessful)
import qualified Network.HTTP.Types.URI as URI
import Network.Wai (Request, getRequestBodyChunk, pathInfo, queryString, rawPathInfo, rawQueryString, remoteHost, requestMethod, responseBuilder)
import qualified Network.Wai.Handler.Warp as Warp
import Prelude hiding (log)
import System.IO (hFlush, stdout)
import System.Posix.Signals (Handler(..), fullSignalSet, installHandler, sigTERM)

type Application = StdMethod -> Request -> IO Response

type ShutdownHandler = IO () -> IO ()

type Response = (Status, Body, Maybe ContentType)

data Body
  = Body ByteString
  | LazyBody Lazy.ByteString

instance IsString Body where
  fromString = Body . fromString

type ContentType = ByteString

listen :: Int -> Application -> IO ()
listen = flip listenWithShutdown onTerminate

onTerminate :: IO () -> IO ()
onTerminate action = void $
  installHandler sigTERM (Catch action') (Just fullSignalSet)
  where
    action' = do
      log ansiRed "terminating"
      action

listenWithShutdown :: Int -> ShutdownHandler -> Application -> IO ()
listenWithShutdown port shutdown route = do
  log ansiBoldYellow $ "listening on localhost:" <> show port
  runWith port $ \request respond -> do
    logRequest request
    let method = parseMethod (requestMethod request)
    (status, headers, body) <- case method of
      Right method -> do
        (status, body, contentType) <- route method request <* hFlush stdout
        let headers = maybeToList $ sequence (hContentType, contentType)
        pure (status, headers, body)
      Left _ ->
        pure (methodNotAllowed405, [], "")
    logResponse status headers
    respond $ responseBuilder status headers $ case body of
      Body content -> Builder.fromByteString content
      LazyBody content -> Builder.fromLazyByteString content
  where
    runWith port = Warp.runSettings $
      Warp.setHost "localhost" $ Warp.setPort port $
      Warp.setInstallShutdownHandler shutdown $
      Warp.defaultSettings
    logRequest request = log ansiBoldWhite $
      ByteString.unpack $ mconcat
        [ requestMethod request
        , " "
        , rawPathInfo request
        , rawQueryString request
        --, "\n" <> ByteString.pack (show request)
        ]
    logResponse status@(Status code message) headers =
      let colour = if statusIsSuccessful status then ansiBoldGreen else ansiBoldRed
      in log colour $ show code <> " - " <> ByteString.unpack message

getRemainingBody :: Request -> IO ByteString
getRemainingBody request = do
  chunk <- getRequestBodyChunk request
  if ByteString.null chunk
    then pure ByteString.empty
    else (chunk <>) <$> getRemainingBody request

ok :: Maybe ContentType -> Body -> Response
ok contentType body = (ok200, body, contentType)

created :: Maybe ContentType -> Body -> Response
created contentType body = (created201, body, contentType)

accepted :: Maybe ContentType -> Body -> Response
accepted contentType body = (accepted202, body, textPlain)

badRequest :: Maybe ContentType -> Body -> Response
badRequest contentType body = (badRequest400, body, contentType)

forbidden :: Maybe ContentType -> Body -> Response
forbidden contentType body = (forbidden403, body, contentType)

notFound :: Maybe ContentType -> Body -> Response
notFound contentType body = (notFound404, body, contentType)

conflict :: Maybe ContentType -> Body -> Response
conflict contentType body = (conflict409, body, contentType)

methodNotAllowed :: Maybe ContentType -> Body -> Response
methodNotAllowed contentType body = (methodNotAllowed405, body, contentType)

internalServerError :: Maybe ContentType -> Body -> Response
internalServerError contentType body = (internalServerError500, body, contentType)

notImplemented :: Maybe ContentType -> Body -> Response
notImplemented contentType body = (notImplemented501, body, contentType)

textPlain :: Maybe ContentType
textPlain = Just "text/plain"

textCsv :: Maybe ContentType
textCsv = Just "text/csv"

textHtml :: Maybe ContentType
textHtml = Just "text/html"

textXml :: Maybe ContentType
textXml = Just "text/xml"

applicationJson :: Maybe ContentType
applicationJson = Just "application/json"

applicationOctetStream :: Maybe ContentType
applicationOctetStream = Just "application/octet-stream"

queryParams :: Request -> Maybe (Map ByteString ByteString)
queryParams  request = Map.fromList <$> sequenceA (sequenceA <$> queryString request)

urlEncode :: Url -> Url
urlEncode = ByteString.unpack . URI.urlEncode True . ByteString.pack

type Url = String
