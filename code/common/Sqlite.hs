{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sqlite
  ( withDatabase
  , withStatement
  , Conflict(..)
  , executeSql
  , executeSqlScalar
  , executeStatements
  , batchInsert
  , batchInsertInternal
  , fromSQLInteger
  , fromSQLText
  , sqlet
  , maybeSqlet
  , executeSqlets
  ) where

import Control.Exception (Exception, bracket, catchJust, throw)
import Control.Monad (when)
import Control.Monad.Trans.Writer (Writer(..), runWriter, tell)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Monoid (Ap(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.SQLite3 (Database, Error(..), SQLData(..), SQLError(..), Statement, StepResult(..))
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

data Conflict = Conflict [SQLError]
  deriving Show

instance Exception Conflict

handleConflict :: IO a -> IO a
handleConflict action =
  attempt [] =<< addUTCTime 1 <$> getCurrentTime
  where
    attempt errors deadline = do
      action `catch` retry errors deadline
    catch = catchJust $ \case
      e@(SQLError ErrorConstraint _ _) -> Just e
      e@(SQLError ErrorLocked _ _) -> Just e
      e@(SQLError ErrorBusy _ _) -> Just e
      _ -> Nothing
    retry errors deadline e = do
      now <- getCurrentTime
      if now >= deadline || sqlError e == ErrorConstraint
        then throw $ Conflict (e : errors)
        else attempt (e : errors) deadline

executeSql :: HasCallStack => Database -> [Text] -> [SQLData] -> IO [[SQLData]]
executeSql database sql params =
  withStatement database (Text.unlines sql) $ \statement -> do
    log ansiMagenta $ Text.unpack $ Text.unwords sql
    log ansiMagenta $ "? " <> show params
    SQLite3.bind statement params
    let fetch = SQLite3.stepNoCB statement >>= \case
          Row -> (:) <$> SQLite3.columns statement <*> fetch
          Done -> pure []
    results <- handleConflict fetch
    log ansiYellow $ "! " <> show results
    pure results

executeSqlScalar :: Database -> [Text] -> [SQLData] -> IO SQLData
executeSqlScalar database sql params = do
  [ [ result ] ] <- executeSql database sql params
  pure result

executeStatements :: Database -> [[Text]] -> IO ()
executeStatements database statements = for_ statements $ flip (executeSql database) []

-- Consider using a list of fixed sized vector instead of `[[SQLData]]`.
batchInsert :: Database -> Text -> [Text] -> [[SQLData]] -> IO ()
batchInsert = batchInsertInternal 999 -- 32766

batchInsertInternal :: Int -> Database -> Text -> [Text] -> [[SQLData]] -> IO ()
batchInsertInternal varLimit database table columns rows = do
  let columnCount = length columns
  let rowCount = length rows
  let batchSize = varLimit `div` columnCount
  let (batchCount, remainderCount) = rowCount `divMod` batchSize
  let commaSep n = Text.intercalate ", " . replicate n
  let withInsertRows n f = flip (withStatement database) f $ Text.unlines
        [ "INSERT INTO " <> table <> "(" <> Text.intercalate ", " columns <> ")"
        , "VALUES " <> n `commaSep` ("(" <> columnCount `commaSep` "?"  <> ")") ]
  let split :: [a] -> ([[a]], [a])
      split xs = case splitAt batchSize xs of
        (ys, zs)
          | length ys < batchSize -> ([], ys)  -- Discovered this case was missing with QuickCheck
          | length zs > batchSize -> first (ys :) (split zs)
          | length zs < batchSize -> ([ ys ], zs)
          | otherwise -> ([ ys, zs ], [])
      (batches, remainder) = split rows
      insert values statement = do
        SQLite3.bind statement values
        Done <- SQLite3.stepNoCB statement
        SQLite3.reset statement
  when (length batches > 0) $ withInsertRows batchSize $ \statement ->
    for_ batches $ \batch -> insert (concat batch) statement
  when (length remainder > 0) $ withInsertRows (length remainder) $
    insert $ concat remainder

fromSQLInteger :: SQLData -> Maybe Int64
fromSQLInteger = \case
  SQLInteger value -> Just value
  SQLNull -> Nothing

fromSQLText :: SQLData -> Maybe Text
fromSQLText = \case
  SQLText value -> Just value
  SQLNull -> Nothing

newtype Sqlet = Sqlet (Writer [SQLData] Text)

instance IsString Sqlet where
  fromString = Sqlet . pure . Text.pack

instance Semigroup Sqlet where
  Sqlet lhs <> Sqlet rhs = Sqlet $ getAp $ Ap lhs <> pure " " <> Ap rhs

instance Monoid Sqlet where
  mempty = Sqlet $ pure ""

sqlet :: Text -> SQLData -> Sqlet
sqlet sql arg = Sqlet $ tell [ arg ] $> sql

maybeSqlet :: Text -> Maybe SQLData -> Sqlet
maybeSqlet sql = \case
  Just arg -> sqlet sql arg
  Nothing -> Sqlet $ pure ""

executeSqlets :: Database -> [Sqlet] -> IO [[SQLData]]
executeSqlets database sqlets =
  let (sql, args) = runWriter $ coerce $ mconcat sqlets
  in executeSql database [ sql ] args
