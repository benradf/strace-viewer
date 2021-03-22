{-# LANGUAGE NumericUnderscores #-}

module Common.Test
  ( Environment
  , withBinaryRunning
  ) where

import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import System.IO (BufferMode(..), IOMode(..), hSetBuffering, withFile)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess)

type Environment = [(String, String)]

withBinaryRunning :: String -> [String] -> Environment -> IO a -> IO a
withBinaryRunning binary args env action = do
  withFile (binary <> ".log") AppendMode $ \logHandle -> do
    hSetBuffering logHandle LineBuffering
    existingEnv <- getEnvironment
    let redirectToLog = UseHandle logHandle
        specification = (proc ("bin/" <> binary) args)
          { std_in = NoStream
          , std_out = redirectToLog
          , std_err = redirectToLog
          , env = Just $ existingEnv <> env
          }
    withCreateProcess specification $ \_ _ _ _ -> do
      threadDelay 100_000
      action
