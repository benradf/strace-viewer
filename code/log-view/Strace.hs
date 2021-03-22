{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Strace
  ( StraceEntry(..)
  , Syscall(..)
  , Signal(..)
  , Exit(..)
  , route
  , importLines
  , importer
  , insert
  , parser
  , withDatabase
  , processes
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (guard, when)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as ByteString
import Data.Either (fromLeft, fromRight)
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.Functor (($>), (<&>), void)
import Data.IORef (atomicModifyIORef, modifyIORef, newIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sortBy, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Ratio((%), Ratio)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime, timeToTimeOfDay)
import Data.Traversable (for)
import Database.SQLite3 (Database, SQLData(..))
import qualified Filesystem
import GHC.Stack (HasCallStack)
import Http.Server
import Log
import Lucid.Base (Html)
import qualified Lucid.Base as Html
import qualified Lucid.Html5 as Html
import Prelude hiding (log)
import qualified Sqlite
import Sqlite hiding (withDatabase)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Directory (removeFile)
import qualified System.INotify as INotify
import System.IO (BufferMode(..), IOMode(..), hClose, hPutStrLn, hSetBuffering, openFile, withFile)
import System.Posix.ByteString.FilePath (RawFilePath)
import System.Posix.Types (ProcessID)
import Text.Printf (printf)
import Text.Read (readMaybe)




hsv :: Integral a => Ratio a -> (Ratio a, Ratio a, Ratio a)
hsv h = (f 0, f 8, f 4)
  where
    f n = l - a * (max (-1) (minimum [k n - 3, 9 - k n, 1]))
    k n = (n + h / 30) `fracMod` 12
    a = s * min l (1 - l) 
    fracMod x n = let (y, z) = properFraction x in fromIntegral (y `mod` n) + z
    l = 1 % 2
    s = 1




colourRects :: IO ()
colourRects = withFile "/tmp/colours.svg" WriteMode $ \file -> do
  hPutStrLn file "<svg version=\"1.1\" baseProfile=\"full\" width=\"100\" height=\"100%\" xmlns=\"http://www.w3.org/2000/svg\">"
  for_ ([ 0 .. 91 ] <&> \n -> let (r, g, b) = hsv (n * (360 % 92)) in ((floor $ 255 * r, floor $ 255 * g, floor $ 255 * b), n)) $ \((r, g, b), i) -> hPutStrLn file $ "<rect x=\"0\" width=\"100\" y=\"" <> show (floor (i * height)) <> "\" height=\"" <> show (floor height) <> "\" fill=\"" <> printf "#%02x%02x%02x" r g b <> "\" />"
  hPutStrLn file "</svg>"
  where
    height = 10
    count = 12








-- TODO: Can name of queryParams be changed to make its key-value check clear?

route :: Database -> Application
route database method request = case pathInfo request of
  [ "strace" ] -> case method of
    GET -> do
      context <- fullContext database
      script <- doesFileExist "strace.js" >>= \case
        True -> Text.pack <$> readFile "strace.js"
        False -> pure $ Text.decodeUtf8 $(embedFile "log-view/strace.js")
      pure $ ok textHtml $ LazyBody $ LazyText.encodeUtf8 $ Html.renderText $ app script context
    POST -> pure $ notImplemented Nothing ""
    _ -> pure $ methodNotAllowed Nothing ""
  [ "strace", uuid ] -> case method of
    GET -> do
      context <- fullContext database
      case modifyContext context =<< queryParams request of
        Nothing -> pure $ badRequest Nothing ""
        Just context -> do
          let getPid [ SQLInteger pid ] = fromIntegral pid
          pids <- getPid <$$> rootProcesses database
          forest <- processes database pids
          grid <- NonEmpty.toList <$$> layout context forest
          let quoted s = "\"" <> s <> "\""
              nullableTime = maybe "null" $ quoted . serialiseTime
              jsonArray elements = "[" <> ByteString.intercalate "," elements <> "]"  -- TODO: Move json helpers to Http
              jsonObject elements = "{" <> ByteString.intercalate "," (elements <&>   -- when they are needed elsewhere.
                \(key, value) -> "\"" <> key <> "\":" <> value) <> "}"
              serialiseKey NodeKey{..} = jsonObject
                [ ("pid", ByteString.pack $ show nodeProcessId)
                , ("start", nullableTime nodeStart)
                , ("end", nullableTime nodeEnd)
                ]
              serialiseNode Node{..} = jsonObject
                [ ("key", serialiseKey nodeKey)
                , ("edges", jsonArray $ serialiseKey <$> nodeEdges)
                , ("command", quoted $ Text.encodeUtf8 nodeCommand)
                ]
              serialiseRow = jsonArray . map serialiseNode
          pure $ ok applicationJson $ Body $ jsonArray $ reverse $ serialiseRow <$> grid
    _ -> pure $ methodNotAllowed Nothing ""
  _ -> pure $ notFound Nothing ""

modifyContext :: Context -> Map ByteString ByteString -> Maybe Context
modifyContext Context{..} params = do
  let parse f key = case f <$> Map.lookup key params of
        Just (Just value) -> Just (Just value)
        Just Nothing -> Nothing
        Nothing -> Just Nothing
  contextStart <- parse deserialiseTime "start"
  contextEnd <- parse deserialiseTime "end"
  pure Context{..}

  -- TODO: Embed minimum and maximum times in application so it knows the suitable range of times for contexts.

  -- TBC:
  --  1. Parse query params to get context
  --  2. Serialise process tree to json

serialiseTime :: TimeOfDay -> ByteString
serialiseTime =
  let convert = toRational . timeOfDayToTime
  in ByteString.pack . show @Rational . convert

deserialiseTime :: ByteString -> Maybe TimeOfDay
deserialiseTime =
  let convert = timeToTimeOfDay . fromRational
  in fmap convert . readMaybe @Rational . ByteString.unpack


data Syscall = Syscall
  { syscallPid :: ProcessID
  , syscallTime :: TimeOfDay
  , syscallName :: Text
  , syscallArgs :: Text
  , syscallReturn :: Text
  }
  deriving (Eq, Show)

data Signal = Signal
  { signalPid :: ProcessID
  , signalTime :: TimeOfDay
  , signalType :: Text
  }
  deriving (Eq, Show)

data Exit = Exit
  { exitPid :: ProcessID
  , exitTime :: TimeOfDay
  , exitCode :: Maybe Int64
  , exitSignal :: Maybe Text
  }
  deriving (Eq, Show)

---

{-
    # Processes that started after strace began recording:
    sqlite3 /tmp/strace.sqlite "
-}
nonRoots :: Database -> IO [[SQLData]]
nonRoots database = executeSql database
  [ "SELECT pid FROM (SELECT DISTINCT(pid) as pid FROM syscall) as process"
  , "LEFT JOIN (SELECT return FROM syscall WHERE name = 'clone') as clone"
  , "ON process.pid = clone.return WHERE clone.return IS NOT NULL;"
  ] []
{-
    "

    # Processes that were already running when strace began recording:
    sqlite3 /tmp/strace.sqlite "
-}
rootProcesses :: Database -> IO [[SQLData]]
rootProcesses database = executeSql database
  [ "SELECT lhs.pid"
  , "FROM process AS lhs"
  , "LEFT JOIN process AS rhs"
  , "ON lhs.ppid = rhs.pid"
  , "WHERE rhs.pid IS NULL;"
  ] []
  --where
    --showTime = SQLText . Text.pack . iso8601Show

-- 

-- SELECT pid FROM (SELECT DISTINCT(pid) as pid FROM syscall) as process LEFT JOIN (SELECT return FROM syscall WHERE name = 'clone') as clone ON process.pid = clone.return WHERE clone.return IS NULL;

{-
    "


    sqlite3 /tmp/strace.sqlite "select return from syscall where name = 'clone' and pid = 32;"
-}



processes :: Database -> [ProcessID] -> IO [Process]
processes database pids = fmap concat $ for pids $ \pid -> do
  let parseTimes :: (TimeOfDay -> a) -> [[SQLData]] -> [a]
      parseTimes f = map $ \[ SQLText time ] ->
        case iso8601ParseM $ Text.unpack time of
          Nothing -> error "parse time failed"
          Just time -> f time
  startTimes <- parseTimes Left <$> executeSql database
    [ "SELECT time FROM syscall WHERE name = 'clone' and return = ?;"
    ] [ SQLText $ Text.pack $ show pid ]
  endTimes <- parseTimes Right <$> executeSql database
    [ "SELECT time FROM exit WHERE pid = ?;"
    ] [ SQLInteger $ fromIntegral pid ]
  let times = flip sortBy (startTimes <> endTimes) $ comparing $ \case
        Left time -> time
        Right time -> time
      Spans spans = flip foldMap times $ Spans . pure . \case
        Left time -> (Just time, Nothing)
        Right time -> (Nothing, Just time)
      spans2 = case spans of          -- }
        [] -> [ (Nothing, Nothing) ]  -- } TODO: Do this more elegantly
        _ -> spans                    -- }
  for spans2 $ \(processStart, processEnd) ->
    let showTime = SQLText . Text.pack . iso8601Show
        syscallPid = fromIntegral pid
        processId = syscallPid
        processSyscalls Context{..} =
          executeSqlets database
            [ "SELECT time, name, args, return FROM syscall"
            , sqlet "WHERE pid = ?" $ SQLInteger $ fromIntegral pid
            , maybeSqlet "AND time >= ?" $ showTime <$> processStart
            , maybeSqlet "AND time < ?" $ showTime <$> processEnd
            , maybeSqlet "AND time >= ?" $ showTime <$> contextStart
            , maybeSqlet "AND time < ?" $ showTime <$> contextEnd
            , sqlet "AND (?" $ SQLInteger $ if Set.null contextSyscalls then 1 else 0
            , flip foldMap contextSyscalls $ \name -> sqlet "OR name = ?" $ SQLText name
            , ");" ] <&&> \
              [ SQLText time
              , SQLText syscallName
              , SQLText syscallArgs
              , SQLText syscallReturn
              ] -> let syscallTime = fromJust $ iso8601ParseM $ Text.unpack time in Syscall{..}
        processChildren Context{..} = do
          pids <- executeSqlets database
            [ "SELECT DISTINCT(return)"
            , "FROM syscall LEFT JOIN exit"
            , "ON syscall.return = exit.pid"
            , "WHERE name = 'clone'"
            , sqlet "AND syscall.pid = ?" $ SQLInteger $ fromIntegral pid
            , maybeSqlet "AND syscall.time >= ?" $ showTime <$> processStart
            , maybeSqlet "AND syscall.time < ?" $ showTime <$> processEnd
-- TODO: Even if a process falls outside the context window, its children may not so it still must be traversed.
--       (but not added to the layout)
            , maybeSqlet "AND (exit.time > ? OR exit.time IS NULL)" $ showTime <$> contextStart
            , maybeSqlet "AND (syscall.time < ? OR syscall.time IS NULL)" $ showTime <$> contextEnd
            , flip foldMap contextHidden $ \p ->
              sqlet "AND return != ?" $ SQLText $ Text.pack $ show p
            , ";" ] <&&> \[ SQLText pid ] -> read $ Text.unpack pid
          processes database pids
    in pure Process{..}

data Spans a = Spans [(Maybe a, Maybe a)]
  deriving Show

instance Show a => Semigroup (Spans a) where
  (<>) = curry $ \case
    (Spans [], rhs) -> rhs
    (lhs, Spans []) -> lhs
    (Spans lhs, Spans rhs) -> case (last lhs, head rhs) of
      ((_, Just end), (Just start, _)) -> Spans $ lhs <> rhs
      ((Just start, Nothing), (Nothing, Just end)) -> Spans $ init lhs <> [(Just start, Just end)] <> tail rhs
      --_ -> error $ "overlapping spans: " <> show lhs <> " <> " <> show rhs
      _ -> mempty  -- TODO: Fix this instead of ignoring it!

instance Show a => Monoid (Spans a) where
  mempty = Spans []

{-

      
-}

--    e   s   e      s   e       s e


-- TO BE CONTINUED (2021/02/07)
test = withDb $ \db -> do { context <- fullContext db; ps <- processForest db context; for_ ps $ \p -> do { putStr $ show (processId p) <> " -> "; putStrLn . show . map processId =<< processChildren p context } }

rootPids = withDb $ \db -> putStrLn . show . map processId =<< processForest db =<< fullContext db

withDb :: (Database -> IO ()) -> IO ()
withDb action = withDatabase "/data/strace.sqlite" $ \database _ -> action database

processForest :: Database -> Context -> IO [Process]
processForest database context@Context{..} =
  processes database $ Set.toList contextRoots


--walk :: Database -> Context -> [Process] -> IO [String]
--walk database context ps = fmap concat $ for ps $ \Process{..} -> do
--  children <- walk database context =<< processChildren context
--  pure $ show processId : map ("    " <>) children
--
--walkTest getContext = withDb $ \db -> do { context <- getContext db; roots <- processForest db context; traverse_ putStrLn =<< walk db context roots }




-- TBC:
-- * Walk process tree and generate partial order :: [(ProcessID, ProcessID)]
-- * Topologically sort processes using the partial order
-- * Layout process graph in this order


--walkTest2 getContext = withDb $ \db -> do { context <- getContext db; grid <- layout context =<< processForest db context; traverse_ (putStrLn . intercalate " ") $ (reverse grid) <&&> \p -> show (processId p) <> "{ " <> show (processStart p) <> " to " <> show (processEnd p) <> " }" }

data NodeKey = NodeKey
  { nodeStart :: Maybe TimeOfDay
  , nodeEnd :: Maybe TimeOfDay
  , nodeProcessId :: ProcessID
  }

data Node = Node
  { nodeKey :: NodeKey
  , nodeCommand :: Text
  , nodeEdges :: [NodeKey]
  }

layout :: Context -> [Process] -> IO [NonEmpty Node]
layout context processes = foldr compose [] <$> do
  let sorted = sortBy $ comparing processStart
  for (sorted processes) $ \process -> do
    children <- processChildren process context
    let toNodeKey Process{..} = NodeKey processStart processEnd processId
        nodeEdges = toNodeKey <$> children
        nodeKey = toNodeKey process
        nodeCommand = ""
    layout context children <&> (<> [ pure Node{..} ])
  where
    compose :: [NonEmpty Node] -> [NonEmpty Node] -> [NonEmpty Node]
    compose = curry $ \case
      ([], []) -> []
      (lhs, []) -> lhs
      ([], rhs) -> rhs
      (lhs, rhs) -> fromMaybe (lhs <> rhs) $ do
        let latest = nodeEnd . nodeKey . NonEmpty.last
        latestLhs <- maximum <$> traverse latest lhs
        let earliest = nodeStart . nodeKey . NonEmpty.head
        earliestRhs <- minimum <$> traverse earliest rhs
        guard $ latestLhs < earliestRhs
        let minLength = min (length lhs) (length rhs)
        pure $ zipWith (<>) lhs rhs <>
          case (length lhs, length rhs) of
            (lenLhs, lenRhs)
              | lenLhs > lenRhs -> drop lenRhs lhs
              | lenLhs < lenRhs -> drop lenLhs rhs
              | otherwise -> []

app :: Text -> Context -> Html ()
app script Context{..} = Html.doctypehtml_ $ do
  Html.head_ $ do
    Html.title_ "strace viewer"
    Html.meta_ [ Html.charset_ "utf-8" ]
    Html.script_ script
  Html.body_ mempty
--    script = lift $ doesFileExist "strace.js" >>= \case
--      True -> Text.pack <$> readFile "strace.js"
--      False -> pure $ Text.decodeUtf8 $(embedFile "log-view/strace.js")

--  Svg.with (Svg.svg11_ content)
--    [ Svg.version_ "1.1"
--    --, Svg.viewBox_ "0 0 1920 1080"
--    , Svg.viewBox_ "0 0 1920 1080"
--    ]
--  where
--    content = do
--      Svg.rect_
--        [ Svg.x_ "100"
--        , Svg.y_ "100"
--        , Svg.width_ "1720"
--        , Svg.height_ "880"
--        , Svg.fill_ "blue"
--        ]
    -- TBC: If either time is Nothing, need to use the max end or min start of all processes.
    --scaleX = 1920 / timeOfDayToTime contextEnd - timeOfDayToTime contextStart
    --timeToX t =
    --lane top height

-- TBC: Write file to disk if it does not exist on startup so it can be modified. If the file on disk is different use that in render (because it has been modified).
-- javascript :: Text
-- javascript = 

-- $ > renderTest fullContext

renderTest :: (Database -> IO Context) -> IO ()
renderTest getContext = withDb $ \db -> do
  context <- getContext db
  grid <- layout context =<< processForest db context
  script <- doesFileExist "strace.js" >>= \case
    True -> Text.pack <$> readFile "strace.js"
    False -> pure $ Text.decodeUtf8 $(embedFile "log-view/strace.js")
  let svg = Text.unpack $ toStrict $ Html.renderText $ app script context
  createDirectoryIfMissing False "/www"
  writeFile "/www/strace.html" svg
  putStrLn svg

-- TODO: Consider allowing processes to be filtered by session id and potentially other process attributes.
-- This could generalise the idea of a simple set of process roots and a set of subtrees to hide.
-- strace?start=8:00&end=8:10&ppid=7&
-- .. actually beginning to think this gains nothing over roots and hidden sets


{-
      location /tmp/ {
        rewrite /tmp/(.*) /$1 break;
        root /tmp;
      }
-}



--layout :: Context -> Process -> IO [[Process]]
--layout context process = foldr compose [ [ process ] ] <$> do
--  let sort = sortBy $ comparing processStart
--  children <- sort <$> processChildren process context
--  for children $ layout context
--  where
--    compose lhs rhs =
--      fromMaybe (lhs <> rhs) $
--      find (not . any collides) $
--      overlap lhs rhs <$> [ 0 .. length lhs - 1 ]
--    overlap lhs rhs offset =
--      let (xs, rhs') = splitAt offset rhs
--      in xs <> zipWith (<>) lhs (rhs' <> repeat [])
--    collides = \case
--      p : ps@(q : _)
--        | (compare <$> processEnd p <*> processStart q) == Just LT -> collides ps
--        | otherwise -> True
--      _ -> False








{-
      StartTime before that of an overlapping process means it must go further from the parent in the graph
-}


{-

SELECT DISTINCT(process.pid)
FROM syscall AS process LEFT JOIN syscall AS clone
ON clone.name = 'clone' AND clone.return = process.pid
WHERE clone.return IS NULL;

syscall (return, name)
syscall (pid)

exit (pid)


-}

-- IDEA: Could create a test that diffs the topological layout of the strace visualization.
-- This would allow the slightest change to which processes start and their order to be noticed.

data Process = Process
  { processId :: ProcessID
  , processStart :: Maybe TimeOfDay
  , processEnd :: Maybe TimeOfDay
  , processSyscalls :: Context -> IO [Syscall]
  , processChildren :: Context -> IO [Process]
  }
  --deriving Show

type ContextId = Int64

type ContextName = Text

data Context = Context
  { contextId :: ContextId
  , contextName :: Maybe ContextName
  , contextStart :: Maybe TimeOfDay
  , contextEnd :: Maybe TimeOfDay
  , contextRoots :: Set ProcessID
  , contextHidden :: Set ProcessID  -- TODO: Think through how show and hiding processes should work exactly.
  , contextSyscalls :: Set Text  -- empty -> all syscalls; otherwise -> inclusion filter
  }                               --        1,2,3,-5,-8,-9,-11
  deriving Show           -- Positive pids are roots, negative are processes to be hidden (and their children)
    -- Start from roots, filter out negative pids from processChildren list

fullContext :: Database -> IO Context
fullContext database = do
  let contextId = 0
      contextName = Nothing
      contextStart = Nothing
      contextEnd = Nothing
      contextHidden = Set.empty
      contextSyscalls = Set.empty
      contextRoots = undefined
  contextRoots <- fmap Set.fromList $ rootProcesses database <&&>
    \[ SQLInteger pid ] -> fromIntegral pid
  pure Context{..}

{-
    docker exec -it $(docker ps -q | head -1) bash -c "
      while true; do
        rm -fv tmp/strace.sqlite
        sqlite3 tmp/strace.sqlite
      done
    "
-}

importLines :: HasCallStack => Database -> ByteString -> [ByteString] -> IO ()
importLines database pid lines = void $ forkIO $ do
  let pairs = lines <&> \line -> (line, Parser.parseOnly parser $ pid <> " " <> line)
  when (length pairs > 0) $ log ansiWhite $ "importing " <> show (length pairs) <>
    " lines (pid " <> ByteString.unpack pid <> ")"
  let warning = \case
        (_, Right entry) -> pure $ Just entry
        (line, Left e) ->
          let message = e <> "\n" <> ByteString.unpack line
          in log ansiYellow message $> Nothing
  insert database =<< (catMaybes <$> traverse warning pairs)

importer :: HasCallStack => RawFilePath -> RawFilePath -> Database -> IO ()
importer root prefix database = void $ do
  inotify <- INotify.initINotify
  INotify.addWatch inotify [ INotify.Create ] root $ \case
    INotify.Created{..}
      | isDirectory -> pure ()
      | otherwise -> case ByteString.stripPrefix prefix filePath of
          Nothing -> pure ()
          Just pid -> do
            let path = ByteString.unpack $ root <> "/" <> filePath
            log ansiGreen $ "opening " <> path
            file <- openFile path ReadMode
            buffer <- newIORef ""
            let batch = do
                  lines <- getLines buffer =<< getAvailable file
                  importLines database pid lines
            watch <- newEmptyMVar
            let handler = \case
                  INotify.Modified{..} -> do
                    batch
                  INotify.Closed{..} -> do
                    log ansiRed $ "closing " <> path
                    INotify.removeWatch =<< readMVar watch
                    hClose file
            putMVar watch =<< INotify.addWatch inotify
              [ INotify.Modify
              , INotify.CloseWrite
              ]  (ByteString.pack path) handler
            batch
  where
    getLines buffer string =
      let (lhs, rhs) = ByteString.break (== '\n') string
      in if rhs == ByteString.empty
          then modifyIORef buffer (<> lhs) $> []
          else do
              line <- atomicModifyIORef buffer $ \partial -> ("", partial <> lhs)
              (line :) <$> getLines buffer (ByteString.tail rhs)
    getAvailable file =
      ByteString.hGetNonBlocking file 0x10000 >>=
        \chunk -> if chunk /= ByteString.empty
          then (chunk <>) <$> getAvailable file
          else pure ByteString.empty


importerTest :: IO ()
importerTest = withDatabase "/tmp/strace.sqlite" $
  \db _ -> importer "/tmp/strace" "output" db

--    lines <- take n . drop m . ByteString.lines <$> ByteString.readFile path
--    putStrLn $ show (length lines) <> " lines read"
--    results <- for lines $ \line ->
--      pure (Parser.parseOnly parser line)
--        -- `finally` ByteString.putStrLn line
--    pure $ sequenceA results
          
  --loadLogs path n m >>= \case
  --  Left e -> error e
  --  Right entries -> insert database entries
  


    -- TBC: 
    -- 1. Add a Modified watch per Created file
    -- 2. Read non-blocking and feed to parser
    -- 3. Add a Closed (writable) watch per Created file and close it ourselves when strace does

  {-
      1. finish inotify import
      2. fix process tree walker
      3. add test for strace api
      4. show process name on hover
      5. fix colour distribution
      6. implement animation
  -}

  --where
    --readChunk parse = ByteString.hGetNonBlocking file 65536 >>= undefined
--      ByteString.empty -> pure ()
--      chunk -> parse
      

insert :: Database -> [StraceEntry] -> IO ()
insert database entries = do
  let filterEntries = flip mapMaybe entries
      syscalls = filterEntries $ \case { StraceSyscall syscall -> Just syscall; _ -> Nothing }
      signals = filterEntries $ \case { StraceSignal signal -> Just signal; _ -> Nothing }
      exits = filterEntries $ \case { StraceExit exit -> Just exit; _ -> Nothing }
  batchInsert database "syscall" [ "pid", "time", "name", "args", "return" ] $
    syscalls <&> \Syscall{..} ->
      [ SQLInteger $ fromIntegral syscallPid
      , SQLText $ Text.pack $ iso8601Show syscallTime
      , SQLText syscallName
      , SQLText syscallArgs
      , SQLText syscallReturn
      ]
  batchInsert database "signal" [ "time", "pid", "type" ] $
    signals <&> \Signal{..} ->
      [ SQLText $ Text.pack $ iso8601Show signalTime
      , SQLInteger $ fromIntegral signalPid
      , SQLText signalType
      ]
  batchInsert database "exit" [ "time", "pid", "code", "signal" ] $
    exits <&> \Exit{..} ->
      [ SQLText $ Text.pack $ iso8601Show exitTime
      , SQLInteger $ fromIntegral exitPid
      , maybe SQLNull SQLInteger exitCode
      , maybe SQLNull SQLText exitSignal
      ]

withDatabase :: FilePath -> (Database -> IO () -> IO ()) -> IO ()
withDatabase path application = Sqlite.withDatabase path $
  \database interrupt -> do
    createSchema database
    application database interrupt

createSchema :: Database -> IO ()
createSchema database = executeStatements database
  [ [ "CREATE TABLE IF NOT EXISTS syscall ("
    , "  pid INTEGER,"
    , "  time TEXT,"
    , "  name TEXT,"
    , "  args TEXT,"
    , "  return TEXT"
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
    , "  type TEXT"
    , ");"
    ]
  , [ "CREATE TABLE IF NOT EXISTS process ("
    , "  pid INTEGER,"
    , "  ppid INTEGER,"
    , "  start TEXT,"
    , "  end TEXT,"
    , "  UNIQUE (pid, end)"
    , ");"
    ]
  , [ "CREATE TRIGGER IF NOT EXISTS process_start"
    , "AFTER INSERT ON syscall WHEN NEW.name = \"clone\" BEGIN"
    , "  INSERT INTO process (pid, ppid, start, end)"
    , "  VALUES (NEW.return, NEW.pid, NEW.time, \"\")"
    , "  ON CONFLICT (pid, end) DO UPDATE SET end = null;"
    , "  INSERT INTO process (pid, ppid, start, end)"
    , "  VALUES (NEW.return, NEW.pid, NEW.time, \"\")"
    , "  ON CONFLICT DO NOTHING;"
    , "END;"
    ]
  , [ "CREATE TRIGGER IF NOT EXISTS process_end"
    , "AFTER INSERT ON exit BEGIN"
    , "  UPDATE process SET end = NEW.time"
    , "  WHERE pid = NEW.pid AND end = \"\";"
    , "  INSERT INTO process (pid, end)"
    , "  VALUES (NEW.pid, NEW.time)"
    , "  ON CONFLICT DO NOTHING;"
    , "END;"
    ]
  , [ "CREATE INDEX IF NOT EXISTS syscall_pid_time ON syscall (pid, time);" ]
  , [ "CREATE INDEX IF NOT EXISTS syscall_name ON syscall (name);" ]
  , [ "CREATE INDEX IF NOT EXISTS syscall_return_name ON syscall (return, name);" ]
  , [ "CREATE INDEX IF NOT EXISTS exit_pid_time ON exit (pid, time);" ]
  ]

--render :: Context -> [Process]
--render = error "not implemented"

--layout :: [Process] -> [[Process]]
--layout = error "not implemented"

--  let spec = (proc "strace-log-merge" [ "/tmp/strace/process" ])
--        { std_in = NoStream , std_out = CreatePipe , std_err = CreatePipe }
--  withCreateProcess spec $ \_ (Just output) (Just error) _ -> do
--    --n <- read . head <$> getArgs

-- TODO: If possible do not split strace into multiple files only to be merged later.
-- Output it to a single file. Then have a process batch read lines and pass them to load-strace.
-- Even better: keep (n, m) as state and run load-strace directly on strace output.
-- It can be implemented as personality of load-strace when it is run without arguments.

{-
    n=100000
    rm -vf /tmp/strace-replication.sqlite &&
    m=0; time while [[ $m -lt 10000000 ]]; do
      printf "$((m + 1)) .. "
      /bin/load-strace /tmp/strace-replication $n $m 1>/dev/null
      m=$((m + n))
      printf "\033[0;32m$m\033[0m\n"
    done
-}


loadLogs :: FilePath -> Int -> Int -> IO (Either String [StraceEntry])
loadLogs path n m = do
  undefined
--    lines <- take n . drop m . ByteString.lines <$> ByteString.readFile path
--    putStrLn $ show (length lines) <> " lines read"
--    results <- for lines $ \line ->
--      pure (Parser.parseOnly parser line)
--        -- `finally` ByteString.putStrLn line
--    pure $ sequenceA results

data StraceEntry
  = StraceSyscall Syscall
  | StraceSignal Signal
  | StraceExit Exit
  deriving (Eq, Show)

{-
  Need to handle:

22:31:54.445652 restart_syscall(<... resuming interrupted read ...> <detached ...>

which happens when newly attaching to a process.

-}

parser :: Parser StraceEntry
parser = do
  Parser.skipSpace
  let integer :: Read a => Parser a
      integer = read <$> Parser.many1 Parser.digit
  pid <- integer
  Parser.skipSpace
  let parseTime = parseTimeM False defaultTimeLocale "%T%Q"
  Just time <- parseTime . ByteString.unpack <$> Parser.takeTill Parser.isSpace
  Parser.skipSpace
  Parser.choice
    [ StraceSyscall <$> do
        let syscallPid = pid
            syscallTime = time
        syscallName <- decodeUtf8 <$> Parser.takeWhile (Parser.inClass "0-9a-z_?")
        let consumeNonParens :: Parser ByteString
            consumeNonParens = mconcat <$> sequenceA
              [ Parser.takeWhile (Parser.notInClass "()\"")
              , Parser.option "" $ mconcat <$> sequenceA
                [ Parser.char '"' $> "\""
                , fmap mconcat $ Parser.many' $ Parser.choice
                  [ ByteString.pack <$> sequenceA [ Parser.char '\\' , Parser.anyChar ]
                  , ByteString.singleton <$> Parser.notChar '"'
                  ]
                , Parser.char '"' $> "\""
                , consumeNonParens
                ]
              ]
        let arguments = mconcat <$> sequenceA
              [ Parser.char '(' $> "("
              , consumeNonParens
              , mconcat <$> Parser.many' ((<>) <$> arguments <*> consumeNonParens)
              , Parser.char ')' $> ")"
              ]
        Parser.choice
          [ do
              syscallArgs <- decodeUtf8 <$> mconcat <$> sequenceA
                [ Parser.char '(' $> "("
                , ByteString.pack <$> Parser.many' (Parser.notChar '<')
                , Parser.choice
                    [ Parser.string "<detached ...>"
                    , Parser.string "<unfinished ...>"
                    ]
                ]
              let syscallReturn = ""
              pure Syscall{..}
          , do
              syscallArgs <- decodeUtf8 <$> arguments
              Parser.skipSpace
              Parser.char '='
              Parser.skipSpace
              syscallReturn <- Text.pack <$> Parser.many' Parser.anyChar
              pure Syscall{..}
          ]
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






          {- TODO: Populate the clone and execve tables with:
                    INSERT INTO clone (id, child_pid)
                    SELECT id, return FROM syscall
                    WHERE name = 'clone';
            So are these tables really necessary at all?
            Should probably get rid of them.
          -}
  --          "clone" -> void $ executeSql database
  --            [ "INSERT INTO clone (id, child_pid)"
  --            , "VALUES (?, ?);" ]
  --            [ SQLInteger id
  --            , SQLInteger $ read $ Text.unpack syscallReturn
  --            ]
  --          "execve" -> pure ()
--  executeSql database
--    [ "INSERT INTO context (name, start, end, hidden, syscalls)"
--    , "SELECT name, start, end, hidden, syscalls FROM context"
--    , "WHERE id = ?;" ] [ SQLInteger id ]  -- TODO: Handle 404 not found
--  let time = "2021-01-31T16:24:00.069705975Z"
--  executeStatements database $ pure <$>
--    [ "insert or replace into context_end (id, time) values (1, \"" <> time <> "\"), (2, \"" <> time <> "\");"
--    , "insert or replace into context_start (id, time) values (2, \"" <> time <> "\"), (3, \"" <> time <> "\");"
--    , "insert or replace into context (id) values (1), (2), (3);"
--    , "insert or replace into named_context (name, id) values (\"foo\", 1), (\"bar\", 2);"
--    ]
--  --putStrLn . show =<< getContextByName database "foo"


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

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)
