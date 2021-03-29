{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as ByteString
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Filesystem
import GHC.Stack (HasCallStack)
import Http.Server
import Log
import Prelude hiding (log)
import qualified Strace
import System.Environment (getArgs, getProgName)

main :: HasCallStack => IO ()
main = do
  setLineBuffering
  path <- fromMaybe "var/run/strace" . listToMaybe <$> getArgs
  Filesystem.createDirectoryMaybe path
  getProgName >>= \case
    "log-view" -> do
      log ansiWhite "log-view service started"
      server path
      log ansiWhite "log-view service stopped"
    "strace-import" -> do
      log ansiWhite $ "strace-import started for " <> path
      Strace.importer (ByteString.pack path) "output."
      log ansiWhite $ "strace-import stopped"

server :: HasCallStack => FilePath -> IO ()
server path = Strace.withDatabase (path <> "/strace.sqlite") $
  \database interrupt -> listenWithShutdown 8083 (shutdown interrupt) $
    \method request -> case pathInfo request of
      "strace" : _ -> Strace.route database method request
      [] -> pure $ if method == GET
        then ok Nothing ""
        else methodNotAllowed Nothing ""
      _ -> pure $ notFound Nothing ""
  where
    shutdown interrupt exit = onTerminate $ interrupt *> exit
