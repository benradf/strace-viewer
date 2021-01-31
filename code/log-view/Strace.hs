{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Strace
  ( main
  ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as ByteString
import Data.Either (fromLeft, fromRight)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (TimeOfDay)
import Data.Traversable (for)
import Database.SQLite3 (Database)
import Http.Server
import qualified Sqlite
import Sqlite hiding (withDatabase)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess)

data Syscall = Syscall
  { syscallPid :: ProcessID
  , syscallTime :: TimeOfDay
  , syscallName :: Text
  , syscallArgs :: Text
  , syscallReturn :: Text
  }
  deriving Show

data Signal = Signal
  { signalPid :: ProcessID
  , signalTime :: TimeOfDay
  , signalType :: Text
  }
  deriving Show

data Exit = Exit
  { exitPid :: ProcessID
  , exitTime :: TimeOfDay
  , exitCode :: Maybe Int64
  , exitSignal :: Maybe Text
  }
  deriving Show

---

data Process = Process
  { processId :: ProcessID
  , processStart :: Maybe TimeOfDay
  , processEnd :: Maybe TimeOfDay
  , processSyscalls :: [Syscall]
  }
  deriving Show

type ContextId = Int64

type ContextName = Text

data Context = Context
  { contextId :: ContextId
  , contextName :: Maybe ContextName
  , contextStart :: Maybe TimeOfDay
  , contextEnd :: Maybe TimeOfDay
  , contextHidden :: Set ProcessID
  , contextSyscalls :: Set Text
  }
  deriving Show

main :: IO ()
main = withDatabase "/tmp/strace.sqlite" $ \database _ -> do
  loadLogs >>= \case
    Left e -> error e
    Right entries -> for entries $ putStrLn . show
--  let time = "2021-01-31T16:24:00.069705975Z"
--  executeStatements database $ pure <$>
--    [ "insert or replace into context_end (id, time) values (1, \"" <> time <> "\"), (2, \"" <> time <> "\");"
--    , "insert or replace into context_start (id, time) values (2, \"" <> time <> "\"), (3, \"" <> time <> "\");"
--    , "insert or replace into context (id) values (1), (2), (3);"
--    , "insert or replace into named_context (name, id) values (\"foo\", 1), (\"bar\", 2);"
--    ]
--  --putStrLn . show =<< getContextByName database "foo"
  pure ()


{-
withNamedContext :: Database -> ContextName -> (Context -> IO a) -> IO a
withNamedContext database name action = do
  context <- getContextByName database name
  action =<< case context of
    Just context -> pure context
    Nothing -> do
      SQLInteger nextId <- executeSqlScalar database
        [ "SELECT MAX(id) + 1 FROM context;" ] []
      executeSql database
        [ "INSERT INTO context (id) VALUES (?);"
        ] [ SQLInteger nextId ]
      executeSql database
        [ "INSERT INTO named_context (name, id) VALUES (?, ?);"
        ] [ SQLText name, SQLInteger nextId ]
      pure $ Context
        { contextId = nextId
        , contextName = Just name
        , contextStart = Nothing
        , contextEnd = Nothing
        , contextHidden = Set.empty
        , contextSyscalls = Set.empty
        }

withCloneContext :: Database -> ContextId -> (Context -> IO a) -> IO (Maybe a)
withCloneContext database id action = loadContext database id >>= \case
  Nothing -> pure Nothing
  Just Context{..} -> do
    undefined

saveContext :: Database -> Context -> IO ()
saveContext database Context{..} = do
  maybeReplace "named_context" "name" contextName
  maybeReplace "context_start" "time" (packTime <$> contextStart)
  maybeReplace "context_end" "time" (packTime <$> contextEnd)
  where
    packTime = Text.pack . iso8601Show
    maybeReplace table column value = void $ sequenceA $ executeSql database
      [ "INSERT OR REPLACE INTO" <> table <> " (id, " <> column <> ")"
      , "VALUES (?, ?);" ] <$> sequenceA
      [ Just $ SQLInteger contextId
      , SQLText <$> value
      ]
--    saveSet table column values = for_ values $ \value ->
--      executeSql database

-- TODO: Continue from here. Need to think a little more about Context model and updates to it.

    

getContextByName :: Database -> ContextName -> IO (Maybe Context)
getContextByName database name = do
  result <- executeSql database
    [ "SELECT id FROM named_context"
    , "WHERE name = ?;" ] [ SQLText name ]
  case result of
    [ [ SQLInteger id ] ] -> loadContext database id
    [] -> pure Nothing

loadContext :: Database -> ContextId -> IO (Maybe Context)
loadContext database id = do
  hidden <- executeSql database
    [ "SELECT pid FROM context_hide"
    , "WHERE id = ?;" ] [ SQLInteger id ]
    <&> map (\[ SQLInteger pid ] -> fromIntegral pid)
  syscalls <- executeSql database
    [ "SELECT syscall FROM context_syscall"
    , "WHERE id = ?;" ] [ SQLInteger id ]
    <&> map (\[ SQLText syscall ] -> syscall)
  result <- executeSql database
    [ "SELECT * FROM context"
    , "LEFT JOIN named_context USING (id)"
    , "LEFT JOIN context_start USING (id)"
    , "LEFT JOIN context_end USING (id)"
    , "WHERE id = ?;" ] [ SQLInteger id ]
  pure $ case result of
    [ [ _, name, start, end ] ] ->
      let contextId = id
          contextName = fromSQLText name
          contextStart = parseTime <$> fromSQLText start
          contextEnd = parseTime <$> fromSQLText end
          contextHidden = Set.fromList hidden
          contextSyscalls = Set.fromList syscalls
      in Just $ Context{..}
    [] -> Nothing
  where
    parseTime = fromJust . iso8601ParseM . Text.unpack

updateContext :: Database -> ContextId -> Query -> IO ContextId
updateContext database id query = do
  executeSql database
    [ "INSERT INTO context (name, start, end, hidden, syscalls)"
    , "SELECT name, start, end, hidden, syscalls FROM context"
    , "WHERE id = ?;" ] [ SQLInteger id ]  -- TODO: Handle 404 not found
  id <- lastInsertRowId database
  let keys = [ "name", "start", "end", "hidden", "syscalls" ]
      value = join . (Map.fromList query !?)
      values = value <$> keys
      column key = value key $> key
      columns = column <$> keys
      param key = value key $> "?"
      params = param <$> keys
  if null columns
    then error "400"
    else error "to be continued"
    
  executeSql database
    [ "UPDATE context SET"
    ] []
  
  pure id
-}

{-

    POST /strace      -- Creates a default context with a unique name (id is returned in body)
    POST /strace/111?start=a&hidden=b,c         -- Clones from 111 and then updates clone with query params
    POST /strace/123?end=d                      -- Clones from 123 then updates clone with query params
    POST /strace/456?name=foo                   -- Clone 456 and name the clone foo (can be used to set current too)

    All above requests return new id in body

    GET /strace/123             -- Renders svg render of context 123

    GET /strace       -- Links for names of contexts in most recently modified (max id) order



TO BE CONTINUED: Design data model around the following REST interface:

    GET /strace                       Returns html with links to `/strace/{id}`
    GET /strace/{id}                  Returns rendered svg with embedded javascript to POST and navigate to new context
    POST /strace                      Returns id of new context with unique name
    POST /strace/{id}?name={name}&start={start}&end={end}&hidden={hidden}&syscalls={syscalls}
                                      Clones id and sets the provided query params (all optional)


    GET /strace
    GET /strace?name={name}&start={start} ...

-}

route :: Application
route method request = do
  -- TODO: Render based on context from query params
  error "not implemented"

--contextual :: String -> (Context


withDatabase :: FilePath -> (Database -> IO () -> IO ()) -> IO ()
withDatabase path application = Sqlite.withDatabase path $
  \database interrupt -> do
    createSchema database
    application database interrupt

createSchema :: Database -> IO ()
createSchema database = executeStatements database
  [ [ "CREATE TABLE IF NOT EXISTS strace ("
    , "  id INTEGER,"
    , "  raw TEXT,"
    , "  pid INTEGER,"
    , "  time TEXT,"
    , "  syscall TEXT,"
    , "  arguments TEXT,"
    , "  return TEXT,"
    , "  PRIMARY KEY (id)"
    , ");"
    ]
  , [ "CREATE TABLE IF NOT EXISTS clone ("
    , "  id INTEGER,"
    , "  child_pid INTEGER,"
    , "  FOREIGN KEY (id) REFERENCES strace(id)"
    , ");"
    ]
  , [ "CREATE TABLE IF NOT EXISTS execve ("
    , "  id INTEGER,"
    , "  path TEXT,"
    , "  FOREIGN KEY (id) REFERENCES strace(id)"
    , ");"
    ]
  , [ "CREATE TABLE IF NOT EXISTS exit ("
    , "  time TEXT,"
    , "  pid INTEGER,"
    , "  code INTEGER,"
    , "  signal TEXT"
    , ");"
    ]
  , [ "CREATE TABLE IF NOT EXISTS signal ("
    , "  time TEXT,"
    , "  pid INTEGER,"
    , "  type TEXT,"
    , "  info TEXT"
    , ");"
    ]
  ]

render :: Context -> [Process]
render = error "not implemented"

layout :: [Process] -> [[Process]]
layout = error "not implemented"


-- strace-log-merge

loadLogs :: IO (Either String [StraceEntry])
loadLogs = do
  let spec = (proc "strace-log-merge" [ "var/log/strace/process" ])
        { std_in = NoStream , std_out = CreatePipe , std_err = CreatePipe }
  withCreateProcess spec $ \_ (Just output) _ _ -> do
    lines <- take 50 . ByteString.lines <$> ByteString.hGetContents output
    putStrLn $ show (length lines) <> " lines read"
    pure $ for lines $ Parser.parseOnly parser


data StraceEntry
  = StraceSyscall Syscall
  | StraceSignal Signal
  | StraceExit Exit
  deriving Show

parser :: Parser StraceEntry
parser = do
  Parser.skipSpace
  let integer :: Read a => Parser a
      integer = read <$> Parser.many1 Parser.digit
  pid <- integer
  Parser.skipSpace
  let parseTime = parseTimeM False defaultTimeLocale "%T%Q"
  Just time <- parseTime . unpack <$> Parser.takeTill Parser.isSpace
  Parser.skipSpace
  Parser.choice
    [ StraceSyscall <$> do
        let syscallPid = pid
            syscallTime = time
        syscallName <- decodeUtf8 <$> Parser.takeWhile (Parser.inClass "0-9a-z")
        let consumeNonParens = Parser.takeWhile (Parser.notInClass "()")
        let arguments = mconcat <$> sequenceA
              [ Parser.char '(' $> "("
              , consumeNonParens
              , mconcat <$> Parser.many' ((<>) <$> arguments <*> consumeNonParens)
              , Parser.char ')' $> ")"
              ]
        syscallArgs <- decodeUtf8 <$> arguments
        Parser.skipSpace
        Parser.char '='
        Parser.skipSpace
        syscallReturn <- decodeUtf8 <$> Parser.takeWhile Parser.isDigit
        pure Syscall{..}
    , StraceExit <$> do
        Parser.string "+++ "
        value <- Parser.choice
          [ do
              Parser.string "exited with "
              Left . Just <$> integer
          , do
              Parser.string "killed by SIG"
              Right . Just . decodeUtf8 <$> Parser.takeTill Parser.isSpace
          ]
        pure $ Exit
          { exitPid = pid
          , exitTime = time
          , exitCode = fromLeft Nothing value
          , exitSignal = fromRight Nothing value
          }
    , StraceSignal <$> do
        Parser.string "--- SIG"
        Signal pid time . decodeUtf8 <$> Parser.takeTill Parser.isSpace
    ]


{-

1337 18:40:19.237811 close(3)                = 0
1337 18:40:19.237972 kill(1338, SIGTERM)     = 0
1338 18:40:19.238053 --- SIGTERM {si_signo=SIGTERM, si_code=SI_USER, si_pid=1337, si_uid=0} ---
1337 18:40:19.238161 select(0, 0x7ffcb8e2af70, 0x7ffcb8e2aff0, NULL, {tv_sec=0, tv_usec=0}) = 0 (Timeout)
1342 18:40:19.238273 +++ killed by SIGTERM +++
1341 18:40:19.238300 +++ killed by SIGTERM +++



64   18:40:10.976548 close(24)               = 0
64   18:40:10.976595 clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7f98c220fd50) = 1097
64   18:40:10.977217 wait4(1097, [{WIFEXITED(s) && WEXITSTATUS(s) == 0}], 0, NULL) = 1097
1097 18:40:10.977268 set_robust_list(0x7f98c220fd60, 24) = 0
1097 18:40:10.977505 ioctl(2, TCGETS, 0x7ffed3fa8e10) = -1 ENOTTY (Inappropriate ioctl for device)
1097 18:40:10.977642 prctl(PR_SET_PDEATHSIG, SIGKILL) = 0
1097 18:40:10.977724 setuid(30001)           = 0
1097 18:40:10.977763 kill(-1, SIGKILL)       = 0
1097 18:40:10.977866 exit_group(0)           = ?
1097 18:40:10.978333 +++ exited with 0 +++
64   18:40:10.978377 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=1097, si_uid=30001, si_status=0, si_utime=0, si_stime=0} ---
64   18:40:10.978412 lstat("/tmp", {st_mode=S_IFDIR|0755, st_size=2, ...}) = 0
64   18:40:10.978479 mkdir("/tmp/nix-build-user-environment.drv-0", 0700) = 0
64   18:40:10.978591 chown("/tmp/nix-build-user-environment.drv-0", 30001, 30000) = 0


12:50:22.839012 lstat("var/log", {st_mode=S_IFDIR|0755, st_size=7, ...}) = 0
12:50:22.839081 openat(AT_FDCWD, "var/log/ssh-keygen.log", O_WRONLY|O_CREAT|O_APPEND, 0644) = 3
12:50:22.839188 dup2(3, 1)              = 1
12:50:22.839242 dup2(3, 2)              = 2
12:50:22.839288 close(3)                = 0
12:50:22.839335 getcwd("/", 4096)       = 2
12:50:22.839383 execve("/bin/ssh-keygen", ["/bin/ssh-keygen", "-A"], 0x94f4d0 /* 7 vars */) = 0
12:50:22.840079 brk(NULL)               = 0x556e643d6000
12:50:22.840164 access("/etc/ld-nix.so.preload", R_OK) = -1 ENOENT (No such file or directory)

-}
