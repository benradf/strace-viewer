module Log
  ( Ansi
  , ansiWhite
  , ansiBoldWhite
  , ansiGreen
  , ansiBoldGreen
  , ansiMagenta
  , ansiRed
  , ansiBoldRed
  , ansiCyan
  , ansiBoldCyan
  , ansiYellow
  , ansiBoldYellow
  , ansiBlue
  , log
  , setLineBuffering
  ) where

import Control.Concurrent (myThreadId)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Stack (HasCallStack, SrcLoc(..), callStack, getCallStack)
import Prelude hiding (log)
import System.Environment (getProgName)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import System.Posix.Process (getProcessID)

data Ansi = Ansi Int Int

ansiWhite :: Ansi
ansiWhite = Ansi 0 37

ansiBoldWhite :: Ansi
ansiBoldWhite = Ansi 1 37

ansiGreen :: Ansi
ansiGreen = Ansi 0 32

ansiBoldGreen :: Ansi
ansiBoldGreen = Ansi 1 32

ansiMagenta :: Ansi
ansiMagenta = Ansi 0 35

ansiRed :: Ansi
ansiRed = Ansi 0 31

ansiBoldRed :: Ansi
ansiBoldRed = Ansi 1 31

ansiCyan :: Ansi
ansiCyan = Ansi 0 36

ansiBoldCyan :: Ansi
ansiBoldCyan = Ansi 1 36

ansiYellow :: Ansi
ansiYellow = Ansi 0 33

ansiBoldYellow :: Ansi
ansiBoldYellow = Ansi 1 33

ansiBlue :: Ansi
ansiBlue = Ansi 0 34

log :: HasCallStack => Ansi -> String -> IO ()
log (Ansi mode colour) string =
  let ansiCode = "\x1b[" <> show mode <> ";" <> show colour <> "m"
      moduleName = srcLocModule $ snd $ head $ getCallStack callStack
      caller = case NonEmpty.nonEmpty $ tail $ getCallStack callStack of
        Just ((caller, _) :| _) -> "." <> caller
        Nothing -> ""
  in do
    location <- getProgName <&> (<> (":" <> moduleName))
    tid <- last . words . show <$> myThreadId
    pid <- ("[" <>) . show <$> getProcessID <&> (<> ("." <> tid <> "] "))
    timestamp <- formatTime defaultTimeLocale "%F %T.%q" <$> getCurrentTime
    let prefix = "\x1b[0;30m" <> pid <> timestamp <> " " <> location <> caller <> "> "
    putStrLn $ prefix <> ansiCode <> string <> "\x1b[0m"

setLineBuffering :: IO ()
setLineBuffering = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
