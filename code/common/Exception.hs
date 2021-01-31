module Exception
  ( IOErrorType(..)
  , catching_
  , catching
  , asNothingWhen
  ) where

import Control.Exception.Base (catchJust)
import GHC.IO.Exception (IOErrorType(..), IOException(..))

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
