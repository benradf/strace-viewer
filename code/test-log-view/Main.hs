{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Time.LocalTime (TimeOfDay(..))
import Strace
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

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
    , HUnit.testCase "Spans" $ withDatabase "testSpans.sqlite" $ \database _ -> do
        let start t = StraceSyscall $ Syscall 0 (read t) "clone" "" "1"
            end t = StraceExit $ Exit 1 (read t) (Just 0) Nothing
        insert database
          [ start "12:58:35", end "13:10:00"
          , start "14:00:00", end "16:00:00"
          , start "19:30:00", end "19:45:00"
          ]
        ps <- processes database [ 1 ]
        insert database
          [ end "12:00:00"
          , start "20:00:00"
          ]
        ps <- processes database [ 1 ]
        pure ()
    ]
  where
    assertParse string expected =
      HUnit.assertEqual "unexpected parse" expected $
      Parser.parseOnly Strace.parser string
