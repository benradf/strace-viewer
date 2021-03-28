{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem
  ( IOErrorType(..)
  , createSymlinkMaybe
  , createDirectoryMaybe
  , removeDirectoryMaybe
  , removeFileMaybe
  , renameFileMaybe
  , openExisting
  , readFileLazily
  ) where

import Control.Error.Util (hush)
import Control.Exception.Base (catchJust)
import qualified Data.ByteString.Lazy as Lazy
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.Directory (createDirectory, removeDirectory, removeFile, renameFile)
import System.IO (Handle, IOMode(..), openBinaryFile)
import System.Posix.Files (createSymbolicLink)

createSymlinkMaybe :: FilePath -> FilePath -> IO (Maybe IOErrorType)
createSymlinkMaybe target path = createSymbolicLink target path `catching_` [ AlreadyExists ]

createDirectoryMaybe :: FilePath -> IO (Maybe IOErrorType)
createDirectoryMaybe path = createDirectory path `catching_` [ AlreadyExists, NoSuchThing ]

removeDirectoryMaybe :: FilePath -> IO (Maybe IOErrorType)
removeDirectoryMaybe path = removeDirectory path `catching_` [ UnsatisfiedConstraints, NoSuchThing ]

removeFileMaybe :: FilePath -> IO (Maybe IOErrorType)
removeFileMaybe path = removeFile path `catching_` [ NoSuchThing ]

renameFileMaybe :: FilePath -> FilePath -> IO (Maybe IOErrorType)
renameFileMaybe old new = renameFile old new `catching_` [ NoSuchThing ]

openExisting :: FilePath -> IO (Either IOErrorType Handle)
openExisting path = openBinaryFile path ReadMode `catching` [ NoSuchThing ]

readFileLazily :: FilePath -> IO (Maybe Lazy.ByteString)
readFileLazily path = hush <$> Lazy.readFile path `catching` [ NoSuchThing ]

catching_ :: IO () -> [IOErrorType] -> IO (Maybe IOErrorType)
catching_ action errors = either Just (const Nothing) <$> action `catching` errors

catching :: IO a -> [IOErrorType] -> IO (Either IOErrorType a)
catching action errors =
  let filter e = if ioe_type e `elem` errors then Just e else Nothing
  in catchJust filter (Right <$> action) (pure . Left . ioe_type)

asNothingWhen :: IO a -> (IOError -> Bool) -> IO (Maybe a)
asNothingWhen action predicate =
  let predicate' e = if predicate e then Just () else Nothing
  in catchJust predicate' (Just <$> action) (const $ pure Nothing)
