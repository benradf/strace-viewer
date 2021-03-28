{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as ByteString
import Data.Maybe (fromMaybe, listToMaybe)
import Database.SQLite3 (Database)
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
  Strace.withDatabase (path <> "/strace.sqlite") $
    \database interrupt -> getProgName >>= \case
      "log-view" -> do
        setLineBuffering
        log ansiWhite "log-view service started"
        server database interrupt
        log ansiWhite "log-view service stopped"
      "strace-import" -> do
        log ansiWhite $ "strace-import started for " <> path
        Strace.importer (ByteString.pack path) "output." database
        threadDelay $ 24 * 60 * 60 * 1000000  -- TODO: Find a better way to block. Perhaps wait until stdin closed?
                                              --       Or wait on an MVar that gets written upon receiving SIGTERM.

server :: HasCallStack => Database -> IO () -> IO ()
server database interrupt = listenWithShutdown 8083 (shutdown interrupt) $
  \method request -> case pathInfo request of
    "strace" : _ -> Strace.route database method request
    [] -> pure $ if method == GET
      then ok Nothing ""
      else methodNotAllowed Nothing ""
    _ -> pure $ notFound Nothing ""
  where
    shutdown interrupt exit = onTerminate $ interrupt *> exit
