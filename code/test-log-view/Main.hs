{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Common.Test
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (join)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor (bimap, first)
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.List.NonEmpty ((<|), NonEmpty(..), fromList, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Data.Ratio ((%))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (DiffTime)
import Data.Time.LocalTime (TimeOfDay(..), timeOfDayToTime)
import Data.Traversable (for)
import qualified Filesystem
import Strace
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode(..), withFile)
import System.Posix.Types (ProcessID)
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

-- $> Main.ghcid

ghcid :: IO ()
ghcid = do
  (pid, lines) <- last <$> generateOutput
  chunks <- chunkOutput lines
  for_ (NonEmpty.take 10 chunks) $ \(delay, chunk) -> do
    putStr $ "\x1b[0;37mafter \x1b[1;37m" <> show (floor $ delay * 10 ^ 6) <> "μs "
    putStrLn $ "\x1b[0;37mwrite chunk to \x1b[1;37moutput." <> show pid <> "\x1b[0;37m:"
    putStrLn $ "\x1b[0;3m" <> Text.unpack chunk <> "\x1b[0;30m|\x1b[0m"
  let path = "/tmp/test-strace-import"
  createDirectoryIfMissing True path
  processes <- generateOutput
  for_ processes $ \(pid, lines) -> do
    chunks <- chunkOutput lines
    writeOutput path pid $ first (floor . (5 *) . ((10 ^ 6) *)) <$> chunks

main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "test-log-view"
    [ HUnit.testCase "parse StraceSyscall" $ assertParse
        "9807  20:37:40.054121 wait4(9818, [{WIFEXITED(s) && WEXITSTATUS(s) == 0}], 0, NULL) = 9818"
        (Right $ StraceSyscall $ Syscall
          { syscallPid = 9807
          , syscallTime = TimeOfDay 20 37 40.054121
          , syscallName = "wait4"
          , syscallArgs = "(9818, [{WIFEXITED(s) && WEXITSTATUS(s) == 0}], 0, NULL)"
          , syscallReturn = "9818"
          }
        )
    , HUnit.testCase "parse StraceSyscall underscore" $ assertParse
        "1914 21:30:24.794306 arch_prctl(ARCH_SET_FS, 0x7fc70fa24040) = 0"
        (Right $ StraceSyscall $ Syscall
          { syscallPid = 1914
          , syscallTime = TimeOfDay 21 30 24.794306
          , syscallName = "arch_prctl"
          , syscallArgs = "(ARCH_SET_FS, 0x7fc70fa24040)"
          , syscallReturn = "0"
          }
        )
    , HUnit.testCase "parse StraceSyscall quotes" $ assertParse
        "1918 21:30:24.904425 read(3, \"\\177ELF\\2\\1\\0)\\0\\0\\3\\0>\\0\\1\\0\\0\\0\\240\\\"\\0\"..., 832) = 832"
        (Right $ StraceSyscall $ Syscall
          { syscallPid = 1918
          , syscallTime = TimeOfDay 21 30 24.904425
          , syscallName = "read"
          , syscallArgs = "(3, \"\\177ELF\\2\\1\\0)\\0\\0\\3\\0>\\0\\1\\0\\0\\0\\240\\\"\\0\"..., 832)"
          , syscallReturn = "832"
          }
        )
    , HUnit.testCase "parse StraceSyscall question marks" $ assertParse
        "1919  22:15:03.957958 ????()                  = ?"
        (Right $ StraceSyscall $ Syscall
          { syscallPid = 1919
          , syscallTime = TimeOfDay 22 15 03.957958
          , syscallName = "????"
          , syscallArgs = "()"
          , syscallReturn = "?"
          }
        )
    , HUnit.testCase "parse StraceSignal" $ assertParse
        "1135  19:46:39.124117 --- SIGTERM {si_signo=SIGTERM, si_code=SI_USER, si_pid=1134, si_uid=0} ---"
        (Right $ StraceSignal $ Signal
          { signalPid = 1135
          , signalTime = TimeOfDay 19 46 39.124117
          , signalType = "TERM"
          }
        )
    , HUnit.testCase "parse StraceExit killed" $ assertParse
        "1135  19:46:39.126567 +++ killed by SIGTERM +++"
        (Right $ StraceExit $ Exit
          { exitPid = 1135
          , exitTime = TimeOfDay 19 46 39.126567
          , exitCode = Nothing
          , exitSignal = Just "TERM"
          }
        )
    , HUnit.testCase "parse StraceExit exited" $ assertParse
        "928   19:46:29.433792 +++ exited with 0 +++"
        (Right $ StraceExit $ Exit
          { exitPid = 928
          , exitTime = TimeOfDay 19 46 29.433792
          , exitCode = Just 0
          , exitSignal = Nothing
          }
        )
    , HUnit.testCase "strace-import" $ do
        let path = "var/run/strace"
        createDirectoryIfMissing True path
        --putStrLn "\x1b[1;35mstrace-import\x1b[0m"
        -- TBC
        withBinaryRunning "strace-import" [ path ] [] $ do
          processes <- generateOutput
          done <- for processes $ \(pid, lines) -> do
            chunks <- chunkOutput lines
            writeOutput path pid $ first (floor . (5 *) . ((10 ^ 6) *)) <$> chunks
          traverse_ takeMVar done
    ]
  where
    assertParse string expected =
      HUnit.assertEqual "unexpected parse" expected $
      Parser.parseOnly Strace.parser string

        -- withBinaryRunning :: String -> [String] -> Environment -> IO a -> IO a

{-

This test creates files mimicking strace output files in a directory being
monitored by strace-import. These files are created at random times over the
course of the test and once created have random length chunks of randomly
generated strace output written to them. Once the file manipulation phase has
finished the test uses the sqlite binary to query all rows from all tables and
checks that the output matches what it expects.

-}

newtype OutputLine = OutputLine [String]

-- strace-import /tmp/test-strace-import

instance Arbitrary OutputLine where
  arbitrary = do
    time <- fmap show $ TimeOfDay
      <$> QuickCheck.chooseBoundedIntegral (0, 23)
      <*> QuickCheck.chooseBoundedIntegral (0, 59)
      <*> (fromRational . toRational <$> (QuickCheck.chooseInt (0, 6 * 10 ^ 6) <&> (% 10 ^ 6)))
    name <- QuickCheck.elements
      [ "access"
      , "bind"
      , "clone"
      , "close"
      , "connect"
      , "dup2"
      , "execve"
      , "exit_group"
      , "fcntl"
      , "fstat"
      , "futex"
      , "getpid"
      , "kill"
      , "listen"
      , "openat"
      , "pipe2"
      , "poll"
      , "read"
      , "wait4"
      , "write"
      ]
    OutputLine <$> QuickCheck.frequency
      [ (1, QuickCheck.elements
          [ "SIGABRT"
          , "SIGCHLD"
          , "SIGHUP"
          , "SIGINT"
          , "SIGKILL"
          , "SIGPIPE"
          , "SIGSEGV"
          , "SIGSTOP"
          , "SIGTERM"
          , "SIGUSR1"
          ] <&> \signal ->
            [ time
            , " +++ killed by "
            , signal
            , " +++"
            ])
      , (1, QuickCheck.elements
          [ "SIGABRT"
          , "SIGCHLD"
          , "SIGHUP"
          , "SIGINT"
          , "SIGKILL"
          , "SIGPIPE"
          , "SIGSEGV"
          , "SIGSTOP"
          , "SIGTERM"
          , "SIGUSR1"
          ] <&> \signal ->
            [ time
            , " +++ exited with 0 +++"
            ])
      , (1, QuickCheck.elements
          [ "SIGABRT"
          , "SIGCHLD"
          , "SIGHUP"
          , "SIGINT"
          , "SIGKILL"
          , "SIGPIPE"
          , "SIGSEGV"
          , "SIGSTOP"
          , "SIGTERM"
          , "SIGUSR1"
          ] <&> \signal ->
            [ time
            , " --- "
            , signal
            , " {si_signo="
            , signal
            , ", si_code=SI_USER, si_pid=1134, si_uid=0} ---"
            ])
      , (17, do
          args <- QuickCheck.elements
            [ "(\"/\", 4096)"
            , "(\"/bin/init\", [\"/bin/init\", \"supervisor\"], 0x7fff8c847d48 /* 4 vars */)"
            , "(\"/etc/ld-nix.so.preload\", R_OK)"
            , "(\"var/log\", {st_mode=S_IFDIR|0755, st_size=18, ...})"
            , "()"
            , "(0x7fd4fe5e1000, 110592, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x7000) "
            , "(1, \"\33[0;30m[1] 2021-03-15 19:40:54.7\"..., 102)"
            , "(1, \"\33[0;30m[6498] 2021-03-15 19:40:5\"..., 121)"
            , "(3)"
            , "(3, 1)"
            , "(3, 2)"
            , "(3, {st_mode=S_IFREG|0555, st_size=336648, ...}) "
            , "(<... resuming interrupted read ...>)"
            , "(AT_FDCWD, \"/etc/localtime\", O_RDONLY|O_CLOEXEC)"
            , "(AT_FDCWD, \"/etc/localtime\", O_RDONLY|O_CLOEXEC)"
            , "(AT_FDCWD, \"var/log/init.log\", O_WRONLY|O_CREAT|O_APPEND, 0644)"
            , "(CLOCK_REALTIME, 0, {tv_sec=1, tv_nsec=0}, 0x7ffce2d0a460)"
            , "(NULL)"
            , "(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7f34026433d0)"
            , "(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fde639f83d0)"
            ]
          return <- QuickCheck.elements
            [ "-1 EADDRINUSE (Address already in use)"
            , "-1 EADDRNOTAVAIL (Cannot assign requested address)"
            , "-1 EAGAIN (Resource temporarily unavailable)"
            , "-1 EBADF (Bad file descriptor)"
            , "-1 ECHILD (No child processes)"
            , "-1 EEXIST (File exists)"
            , "-1 EINPROGRESS (Operation now in progress)"
            , "-1 EINTR (Interrupted system call)"
            , "-1 EINVAL (Invalid argument)"
            , "-1 ENOENT (No such file or directory)"
            , "-1 ENOTTY (Inappropriate ioctl for device)"
            , "-1 ESPIPE (Illegal seek)"
            , "0 (Timeout)"
            , "0"
            , "0x2 (flags O_RDWR)"
            , "0x7ffb78c00000"
            , "0x8000 (flags O_RDONLY|O_LARGEFILE)"
            , "0xccd000"
            , "1 ([{fd=15, revents=POLLOUT|POLLWRNORM}])"
            , "1"
            ]
          pure
            [ time
            , " "
            , name
            , args
            , " = "
            , return
            ]
        )
      ]

generateOutput :: IO [(ProcessID, NonEmpty OutputLine)]
generateOutput = QuickCheck.generate $ do
  lines <- sequenceA $ NonEmpty.fromList $ replicate 86400 $ arbitrary @OutputLine
  let sortByTime = NonEmpty.sortBy $ comparing $ head @String . coerce
      splitHours xs = case bimap fromList nonEmpty $ NonEmpty.splitAt 3600 xs of
        (ys, Just zs) -> ys <| splitHours zs
        (ys, Nothing) -> ys :| []
      arbitraryPid base = fromInteger <$> QuickCheck.chooseInteger (toInteger base, toInteger base + 4)
      assignProcesses lines = sequenceA $ NonEmpty.zip [ 1 .. 24 ] lines <&> \(basePid, lines) -> do
        pids <- sequenceA $ NonEmpty.repeat $ arbitraryPid basePid
        pure $ NonEmpty.zip pids lines
  pairs <- fmap sconcat $ assignProcesses $ splitHours $ sortByTime lines
  let groupByPid :: [(ProcessID, OutputLine)] -> [NonEmpty (ProcessID, OutputLine)]
      groupByPid = NonEmpty.groupBy (curry $ uncurry (==) . join bimap fst) . sortBy (comparing fst)
      sequenceProcessGroup :: NonEmpty (ProcessID, OutputLine) -> (ProcessID, NonEmpty OutputLine)
      sequenceProcessGroup group@((pid, _) :| _) = (pid, snd <$> group)
  pure $ map sequenceProcessGroup $ groupByPid $ NonEmpty.toList pairs

chunkOutput :: NonEmpty OutputLine -> IO (NonEmpty (DiffTime, Text))
chunkOutput lines = QuickCheck.generate $ do
  let extractTime = read @TimeOfDay . head &&& NonEmpty.fromList . (<> pure "\n")
  chunks <- randomlyChunk $ sequenceA . extractTime . coerce =<< lines
  pure $ differences 0 chunks
  where
    randomlyChunk :: NonEmpty (TimeOfDay, String) -> QuickCheck.Gen (NonEmpty (TimeOfDay, Text))
    randomlyChunk atoms = do
      size <- QuickCheck.getPositive <$> QuickCheck.resize 16 arbitrary
      let (lhs@((time, _) : _), rhs) = NonEmpty.splitAt size atoms
          chunk = (time, Text.pack $ concat $ snd <$> lhs)
      case nonEmpty rhs of
          Just rhs -> (chunk <|) <$> randomlyChunk rhs
          Nothing -> (chunk :|) <$> pure []
    differences :: DiffTime -> NonEmpty (TimeOfDay, Text) -> NonEmpty (DiffTime, Text)
    differences t ((time, text) :| chunks) =
      let t' = timeOfDayToTime time / 86400
      in case nonEmpty chunks of
          Just chunks -> (t' - t, text) <| differences t' chunks
          Nothing -> (t' - t, text) :| []

type Microseconds = Int

writeOutput :: FilePath -> ProcessID -> NonEmpty (Microseconds, Text) -> IO (MVar ())
writeOutput directory pid ((initialDelay, chunk) :| chunks) = do
  done <- newEmptyMVar
  forkIO $ do
    threadDelay initialDelay
    let path = directory <> "/output." <> show pid
    Filesystem.removeFileMaybe path
    withFile path AppendMode $ \file -> do
      Text.hPutStr file chunk
      for_ chunks $ \(delay, chunk) -> do
        threadDelay delay
        Text.hPutStr file chunk
    putMVar done ()
  pure done

-- tail -f var/run/strace/output.{1..20}
-- Run test in slow motion and watch the order in which output files are created and written to.

-- TODO: Make strace-import multithreaded ...

-- NOTE: Seem to be failing to close some output files in strace-import

test123 = withBinaryRunning "" [] [] $ pure ()

-- TBC: Launch strace-import and write random chunks for it to import.
-- Then, invoke `diff` on `sqlite3` output and expected output.
-- Or possibly compare the lines in test would be better than using `diff` ..

-- strace-import /tmp/test-strace-output

{-

$ sqlite3 -nullvalue NULL var/run/strace/strace.sqlite "select * from process;"
6172|1|09:21:50.183835|09:21:54.796624
6173|6172|09:21:50.187684|09:21:52.73724
6174|6172|09:21:51.188144|09:21:51.200144
6175|6172|09:21:51.201666|09:21:51.234367
6176|6175|09:21:51.228359|09:21:51.233756
6177|6175|09:21:51.230234|09:21:51.234339
6178|6175|09:21:51.230374|09:21:51.233737
6179|6175|09:21:51.230691|09:21:51.233715
6180|6175|09:21:51.230899|09:21:51.233667
6181|6175|09:21:51.231142|09:21:51.233838
6182|6175|09:21:51.231346|09:21:51.233627
6183|6175|09:21:51.231509|09:21:51.233801
6184|6172|09:21:51.234552|09:21:51.698346
6186|6184|09:21:51.261635|09:21:51.697631
6187|6184|09:21:51.263708|09:21:51.697609
6188|6184|09:21:51.263968|09:21:51.697588
6189|6184|09:21:51.264105|09:21:51.697567
6190|6184|09:21:51.264244|09:21:51.697544
6191|6184|09:21:51.264434|09:21:51.697522
6192|6184|09:21:51.264605|09:21:51.697496
6193|6184|09:21:51.26477|09:21:51.697455
6194|6184|09:21:51.273819|09:21:51.694713
6195|6194|09:21:51.276244|09:21:51.296016
6206|6194|09:21:51.3963|09:21:51.412142
6228|6194|09:21:51.588797|09:21:51.610809
6230|6229|09:21:51.681105|09:21:51.693144
6231|6229|09:21:51.68319|09:21:51.693116
6232|6229|09:21:51.683423|09:21:51.693088
6233|6229|09:21:51.683654|09:21:51.693867
6234|6229|09:21:51.683862|09:21:51.693061
6235|6229|09:21:51.684072|09:21:51.693018
6236|6229|09:21:51.684265|09:21:51.693205
6237|6229|09:21:51.684491|09:21:51.693179
6229|NULL|NULL|09:21:51.693897
6238|6172|09:21:51.698536|09:21:51.708322
6239|6172|09:21:51.711501|
6240|6172|09:21:51.712562|09:21:51.729263
6241|6172|09:21:52.71297|09:21:52.720565
6242|6172|09:21:52.720858|09:21:54.786885
6243|6172|09:21:52.721231|09:21:54.796321
6244|6172|09:21:52.721492|09:21:54.796147
6247|6242|09:21:52.726036|09:21:54.786361
6249|6242|09:21:52.726771|09:21:54.783594
6252|6242|09:21:52.727767|09:21:54.783448
6246|6243|09:21:52.725607|09:21:54.79591
6248|6243|09:21:52.726343|09:21:54.785858
6250|6243|09:21:52.727043|09:21:54.786004
6245|6244|09:21:52.725273|09:21:54.7956
6251|6244|09:21:52.727475|09:21:54.788368
6254|6244|09:21:52.728457|09:21:54.788476
6253|6250|09:21:52.72794|09:21:54.78614
6255|6252|09:21:52.728716|09:21:54.783379
6257|6254|09:21:52.729611|09:21:54.788571
6260|6255|09:21:52.753039|09:21:54.78356
6258|6256|09:21:52.733993|09:21:52.754909
6256|NULL|NULL|09:21:52.755267
6259|6258|09:21:52.741468|09:21:52.752261
6261|6239|09:21:52.834046|
6262|6239|09:21:52.83465|
6263|6239|09:21:52.835189|
6264|6239|09:21:52.835546|
6265|6239|09:21:52.835865|
6305|6253|09:21:54.784609|09:21:54.78606
6307|6257|09:21:54.787195|09:21:54.788532

-}
