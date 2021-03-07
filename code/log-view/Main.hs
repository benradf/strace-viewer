{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite3 (Database)
import GHC.Stack (HasCallStack)
import Http.Server
import Log
import Prelude hiding (log)
import qualified Strace

main :: HasCallStack => IO ()
main = do
  setLineBuffering
  log ansiWhite "log-view service started"
  Strace.withDatabase "/data/strace.sqlite" server
  log ansiWhite "log-view service stopped"

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
