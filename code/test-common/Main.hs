{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import Database.SQLite3 (SQLData(..))
import Sqlite
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Gen (generate)
import Test.QuickCheck.Modifiers (NonNegative(..), Positive(..))
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

data BatchInsert = BatchInsert
  { batchInsertRows :: [[SQLData]]
  , batchInsertVarLimit :: Int
  }
  deriving Show

instance Arbitrary BatchInsert where
  arbitrary = do
    let arbitraryRow = map SQLInteger <$> QuickCheck.vector 5
    batchInsertRows <- replicate . getPositive <$> arbitrary <*> arbitraryRow
    batchInsertVarLimit <- getNonNegative <$> arbitrary <&> (+ 5)
    pure $ BatchInsert{..}

main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "test-common"
    [ HUnit.testCase "Sqlite.batchInsert" $
        withDatabase "batchInsert.sqlite" $ \database _ -> do
          executeSql database
            [ "CREATE TABLE IF NOT EXISTS test ("
            , "  one INTEGER,"
            , "  two INTEGER,"
            , "  three INTEGER,"
            , "  four INTEGER,"
            , "  five INTEGER"
            , ");"
            ] []
          batchInserts <- generate $ QuickCheck.vector 100
          for_ batchInserts $ \BatchInsert{..} -> do
            putStrLn $ "batchInsertVarLimit = " <> show batchInsertVarLimit
            putStrLn $ "batchInsertRows = " <> show batchInsertRows
            batchInsertInternal batchInsertVarLimit database
              "test" [ "one", "two", "three", "four", "five" ]
              batchInsertRows
            results <- executeSql database [ "SELECT * FROM test;" ] []
            HUnit.assertEqual "" batchInsertRows results
            executeSql database [ "DELETE FROM test;" ] []
    ]
