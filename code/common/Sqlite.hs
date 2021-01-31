{-# LANGUAGE LambdaCase #-}

module Sqlite
  ( withDatabase
  , withStatement
  , executeSql
  , executeSqlScalar
  , executeStatements
  , fromSQLInteger
  , fromSQLText
  ) where

import Control.Exception (bracket)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite3 (Database, SQLData(..), Statement, StepResult(..))
import qualified Database.SQLite3 as SQLite3
import GHC.Stack (HasCallStack)
import Log
import Prelude hiding (log)

withDatabase :: FilePath -> (Database -> IO () -> IO ()) -> IO ()
withDatabase path application =
  let open = SQLite3.open $ Text.pack path
  in bracket open SQLite3.close $ \database -> do
    application database $ do
      SQLite3.interrupt database
      log ansiMagenta "interrupt"

withStatement :: Database -> Text -> (Statement -> IO a) -> IO a
withStatement database sql action =
  bracket (SQLite3.prepare database sql) SQLite3.finalize action

executeSql :: HasCallStack => Database -> [Text] -> [SQLData] -> IO [[SQLData]]
executeSql database sql params =
  withStatement database (Text.unlines sql) $ \statement -> do
    log ansiMagenta $ Text.unpack $ Text.unwords sql
    log ansiMagenta $ "? " <> show params
    SQLite3.bind statement params
    let fetch = SQLite3.step statement >>= \case
          Row -> (:) <$> SQLite3.columns statement <*> fetch
          Done -> pure []
    results <- fetch
    log ansiYellow $ "! " <> show results
    pure results

executeSqlScalar :: Database -> [Text] -> [SQLData] -> IO SQLData
executeSqlScalar database sql params = do
  [ [ result ] ] <- executeSql database sql params
  pure result

executeStatements :: Database -> [[Text]] -> IO ()
executeStatements database statements = for_ statements $ flip (executeSql database) []

fromSQLInteger :: SQLData -> Maybe Int64
fromSQLInteger = \case
  SQLInteger value -> Just value
  SQLNull -> Nothing

fromSQLText :: SQLData -> Maybe Text
fromSQLText = \case
  SQLText value -> Just value
  SQLNull -> Nothing
