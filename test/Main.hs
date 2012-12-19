{-# LANGUAGE ScopedTypeVariables #-}

import StrictEq

import Database.SQLite3
import qualified Database.SQLite3.Direct as Direct

import Control.Exception
import Control.Monad        (forM_, when)
import Data.Text            (Text)
import Data.Text.Encoding.Error (UnicodeException(..))
import System.Directory
import System.Exit          (exitFailure)
import System.IO
import System.IO.Error      (isDoesNotExistError, isUserError)
import Test.HUnit

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

data TestEnv =
  TestEnv {
    conn :: Database
    -- ^ Database shared by all the tests
  , withConn :: forall a. (Database -> IO a) -> IO a
    -- ^ Bracket for spawning an additional connection.
    --   This connection will be isolated from others.
  , withConnShared :: forall a. (Database -> IO a) -> IO a
    -- ^ Like 'withConn', but every invocation shares the same database.
  }

regressionTests :: [TestEnv -> Test]
regressionTests =
    [ TestLabel "Exec"          . testExec
    , TestLabel "Simple"        . testSimplest
    , TestLabel "Prepare"       . testPrepare
    , TestLabel "CloseBusy"     . testCloseBusy
    , TestLabel "Params"        . testBind
    , TestLabel "Params"        . testBindParamCounts
    , TestLabel "Params"        . testBindParamName
    , TestLabel "Params"        . testBindErrorValidation
    , TestLabel "Columns"       . testColumns
    , TestLabel "Errors"        . testErrors
    , TestLabel "Integrity"     . testIntegrity
    , TestLabel "DecodeError"   . testDecodeError
    ]

featureTests :: [TestEnv -> Test]
featureTests =
    [ TestLabel "MultiRowInsert" . testMultiRowInsert
    ]

assertFail :: IO a -> Assertion
assertFail action =
  shouldFail action >>= assertBool "assertFail"

-- | Return 'True' if the IO action throws a 'userError',
-- which happens when 'fail' is used.
shouldFail :: IO a -> IO Bool
shouldFail action = do
  r <- try action
  case r of
    Left e  -> return $ isUserError e
    Right _ -> return False

withStmt :: Database -> Text -> (Statement -> IO a) -> IO a
withStmt conn sql = bracket (prepare conn sql) finalize

testExec :: TestEnv -> Test
testExec TestEnv{..} = TestCase $ do
  exec conn ""
  exec conn "     "
  exec conn ";"
  exec conn " ; ; ; ; ; "
  exec conn "--"
  Left SQLError{sqlError = ErrorError} <- try $ exec conn "/*"
    -- sqlite3_exec does not allow "/*" to be terminated by end of input,
    -- but <http://www.sqlite.org/lang_comment.html> says it's fine.
  exec conn ";--\n;/**/"
  withConn $ \conn -> do
    -- Make sure all the statements passed to exec are executed.
    -- Test a little value conversion while we're at it.
    exec conn "CREATE TABLE foo (n FLOAT, t TEXT); \
              \INSERT INTO foo VALUES (3.5, null); \
              \INSERT INTO foo VALUES (null, 'Ự₦ⓘ₡ợ₫ḝ'); \
              \INSERT INTO foo VALUES (null, ''); \
              \INSERT INTO foo VALUES (null, 'null'); \
              \INSERT INTO foo VALUES (null, null)"
    withStmt conn ("SELECT * FROM foo") $ \stmt -> do
      Row <- step stmt
      [SQLFloat 3.5, SQLNull]       <- columns stmt
      Row <- step stmt
      [SQLNull, SQLText "Ự₦ⓘ₡ợ₫ḝ"]  <- columns stmt
      Row <- step stmt
      [SQLNull, SQLText ""]         <- columns stmt
      Row <- step stmt
      [SQLNull, SQLText "null"]     <- columns stmt
      Row <- step stmt
      [SQLNull, SQLNull]            <- columns stmt
      Done <- step stmt
      return ()

-- Simplest SELECT
testSimplest :: TestEnv -> Test
testSimplest TestEnv{..} = TestCase $ do
  stmt <- prepare conn "SELECT 1+1"
  Row <- step stmt
  res <- column stmt 0
  Done <- step stmt
  finalize stmt
  assertEqual "1+1" (SQLInteger 2) res

testPrepare :: TestEnv -> Test
testPrepare TestEnv{..} = TestCase $ do
  True <- shouldFail $ prepare conn ""
  True <- shouldFail $ prepare conn ";"
  withConn $ \conn -> do
    withStmt conn
             "CREATE TABLE foo (a INT, b INT); \
             \INSERT INTO foo VALUES (1, 2); \
             \INSERT INTO foo VALUES (3, 4)"
             $ \stmt -> do
      Done <- step stmt
      return ()
    withStmt conn
             "BEGIN; INSERT INTO foo VALUES (5, 6); COMMIT"
             $ \stmt -> do
      Done <- step stmt
      return ()
    withStmt conn
             "SELECT * FROM foo"
             $ \stmt -> do
      Done <- step stmt -- No row was inserted, because only the CREATE TABLE
                        -- statement was run.  The rest was ignored.
      return ()
    Left SQLError{sqlError = ErrorError} <- try $ exec conn "BEGIN"
      -- We're in a transaction already, so this fails.
    exec conn "COMMIT"
  return ()

testCloseBusy :: TestEnv -> Test
testCloseBusy _ = TestCase $ do
  conn <- open ":memory:"
  stmt <- prepare conn "SELECT 1"
  Left SQLError{sqlError = ErrorBusy} <- try $ close conn
  finalize stmt
  close conn

testBind :: TestEnv -> Test
testBind TestEnv{..} = TestCase $ do
  bracket (prepare conn "SELECT ?") finalize testBind1
  bracket (prepare conn "SELECT ?+?") finalize testBind2
  where
    testBind1 stmt = do
      let params =  [SQLInteger 3]
      bind stmt params
      Row <- step stmt
      res <- columns stmt
      Done <- step stmt
      assertEqual "single param" params res

    testBind2 stmt = do
      let params =  [SQLInteger 1, SQLInteger 1]
      bind stmt params
      Row <- step stmt
      res <- columns stmt
      Done <- step stmt
      assertEqual "two params param" [SQLInteger 2] res

-- Test bindParameterCount
testBindParamCounts :: TestEnv -> Test
testBindParamCounts TestEnv{..} = TestCase $ do
  testCase "single $a"                  "SELECT $a"                     1
  testCase "3 unique ?NNNs"             "SELECT (?1+?1+?1+?2+?3)"       3
  testCase "3 positional"               "SELECT (?+?+?)"                3
  testCase "5 params, 2 gaps"           "SELECT ?3, ?5, ?1"             5
  testCase "6 params, gaps & auto"      "SELECT ?3, ?5, ?1, ?"          6
  testCase "8 params, auto & overlap"   "SELECT ?, ?5, ?, ?2, ?, ?6, ?" 8
    -- 8 because ? grabs an index one greater than the highest index of all
    -- previous parameters, not just the most recent index.
  testCase "0 placeholders"             "SELECT 1"                      0
  where
    testCase label query expected =
        bracket (prepare conn query) finalize bindParameterCount
            >>= assertEqual label expected

-- Test bindParameterName
testBindParamName :: TestEnv -> Test
testBindParamName TestEnv{..} = TestCase $ do
  bracket (prepare conn "SELECT :v + :v2") finalize (testNames [Just ":v", Just ":v2"])
  bracket (prepare conn "SELECT ?1 + ?1") finalize (testNames [Just "?1"])
  bracket (prepare conn "SELECT ?1 + ?2") finalize (testNames [Just "?1", Just "?2"])
  bracket (prepare conn "SELECT ? + ?") finalize (testNames [Nothing, Nothing])
  bracket (prepare conn "SELECT $1 + $2") finalize (testNames [Just "$1", Just "$2"])
  where
    testNames names stmt = do
      count <- bindParameterCount stmt
      assertEqual "count match" count (fromIntegral $ length names)
      mapM_ (\(ndx,expecting) -> do
                name <- bindParameterName stmt ndx
                assertEqual "name match" expecting name) $ zip [1..] names

testBindErrorValidation :: TestEnv -> Test
testBindErrorValidation TestEnv{..} = TestCase $ do
  bracket (prepare conn "SELECT ?") finalize (assertFail . testException1)
  bracket (prepare conn "SELECT ?") finalize (assertFail . testException2)
  where
    -- Invalid use, one param in q string, none given
    testException1 stmt = bind stmt []
    -- Invalid use, one param in q string, 2 given
    testException2 stmt = bind stmt [SQLInteger 1, SQLInteger 2]

testColumns :: TestEnv -> Test
testColumns TestEnv{..} = TestCase $ do
  withConn $ \conn -> do
    withStmt conn "CREATE TABLE foo (a INT)" command
    withStmt conn "SELECT * FROM foo" $ \stmt -> do
      1 <- columnCount stmt
      exec conn "ALTER TABLE foo ADD COLUMN b INT"
      Done <- step stmt
      2 <- columnCount stmt
      return ()
    withStmt conn "SELECT * FROM foo" $ \stmt -> do
      2 <- columnCount stmt
      Done <- step stmt
      2 <- columnCount stmt
      return ()
    withStmt conn "INSERT INTO foo VALUES (1, 2)" command
    withStmt conn "SELECT * FROM foo" $ \stmt -> do
      2 <- columnCount stmt
      Row <- step stmt
      2 <- columnCount stmt
      [SQLInteger 1, SQLInteger 2] <- columns stmt
      Done <- step stmt
      2 <- columnCount stmt
      return ()
    withStmt conn "INSERT INTO foo VALUES (3, 4)" command
    withStmt conn "INSERT INTO foo VALUES (5, 6)" command
    withStmt conn "SELECT * FROM foo" $ \stmt -> do
      2 <- columnCount stmt
      exec conn "ALTER TABLE foo ADD COLUMN c INT"
      Row <- step stmt
      3 <- columnCount stmt
      [SQLInteger 1, SQLInteger 2, SQLNull] <- columns stmt
      exec conn "ALTER TABLE foo ADD COLUMN d INT NOT NULL DEFAULT 42"
        -- ignored by this prepared statement, now that it has stepped.
      Row <- step stmt
      3 <- columnCount stmt
      [SQLInteger 3, SQLInteger 4, SQLNull] <- columns stmt
      Row <- step stmt
      3 <- columnCount stmt
      [SQLInteger 5, SQLInteger 6, SQLNull] <- columns stmt
      Done <- step stmt
      3 <- columnCount stmt
      reset stmt
      3 <- columnCount stmt -- The prepared statement *still* doesn't know
                            -- about the new column.
      Row <- step stmt
      4 <- columnCount stmt -- That's better.
      [SQLInteger 1, SQLInteger 2, SQLNull, SQLInteger 42] <- columns stmt
      return ()
  where
    command stmt = do
      0 <- columnCount stmt
      Done <- step stmt
      0 <- columnCount stmt
      return ()

-- Testing for specific error codes:
--
--  * ErrorConstraint
--
--  * ErrorRange
--
--  * ErrorLocked

--  * ErrorBusy
testErrors :: TestEnv -> Test
testErrors TestEnv{..} = TestCase $ do
  withConn $ \conn -> do
    exec conn "CREATE TABLE foo (n INT UNIQUE)"
    exec conn "INSERT INTO foo VALUES (3)"
    expectError ErrorConstraint $
      exec conn "INSERT INTO foo VALUES (3)"

    -- Multiple NULLs are allowed when there's a UNIQUE constraint
    exec conn "INSERT INTO foo VALUES (null)"
    exec conn "INSERT INTO foo VALUES (null)"

    exec conn "CREATE TABLE bar (n INT NOT NULL)"
    expectError ErrorConstraint $
      exec conn "INSERT INTO bar VALUES (null)"

    withStmt conn "SELECT ?" $ \stmt -> do
      forM_ [-1, 0, 2] $ \i -> do
        expectError ErrorRange $ bindSQLData stmt i $ SQLInteger 42
        expectError ErrorRange $ bindSQLData stmt i SQLNull
      bindSQLData stmt 1 $ SQLInteger 42
      Row <- step stmt

      -- If column index is out of range, it returns SQLNull.
      -- This may or may not be the desired behavior, but at least we know.
      SQLNull <- column stmt (-1)
      SQLNull <- column stmt 1

      SQLInteger 42 <- column stmt 0
      return ()

    withStmt conn "SELECT 1" $ \stmt -> do
      forM_ [-1, 0, 1, 2] $ \i -> do
        expectError ErrorRange $ bindSQLData stmt i $ SQLInteger 42
        expectError ErrorRange $ bindSQLData stmt i SQLNull
      bind stmt []  -- This should succeed.  Don't whine that there aren't any
                    -- parameters to bind!
      Row <- step stmt
      SQLInteger 1 <- column stmt 0
      return ()

    withStmt conn "SELECT ?5" $ \stmt -> do
      forM_ [-1, 0, 6, 7] $ \i -> do
        expectError ErrorRange $ bindSQLData stmt i $ SQLInteger 42
        expectError ErrorRange $ bindSQLData stmt i SQLNull
      bind stmt $ map SQLInteger [1..5]
        -- This succeeds, even though 1..4 aren't used.
      Row <- step stmt
      [SQLInteger 5] <- columns stmt
      return ()

  -- Need to access the database with multiple connections.
  -- "BEGIN; ROLLBACK" causes running statements in the same connection to
  -- throw SQLITE_ABORT.
  withConnShared $ \conn -> do
    foo123456 conn
    withStmt conn "SELECT * FROM foo" $ \stmt -> do
      -- "DROP TABLE foo" should succeed, since the statement
      -- isn't running yet.
      exec conn "DROP TABLE foo"
      foo123456 conn

      Row <- step stmt
      2 <- columnCount stmt
      [SQLInteger 1, SQLInteger 2] <- columns stmt

      -- "DROP TABLE foo" should fail, now that the statement is running.
      expectError ErrorLocked $ exec conn "DROP TABLE foo"
      withConnShared $ \conn -> do
        expectError ErrorBusy $ exec conn "DROP TABLE foo"

        -- Apparently, we can pretend to drop the table, but we get ErrorBusy
        -- if we try to actually COMMIT it.
        exec conn "BEGIN; DROP TABLE foo"
        expectError ErrorBusy $ exec conn "COMMIT"

        exec conn "ROLLBACK"

      Row <- step stmt
      2 <- columnCount stmt
      [SQLInteger 3, SQLInteger 4] <- columns stmt
      Row <- step stmt
      2 <- columnCount stmt
      [SQLInteger 5, SQLInteger 6] <- columns stmt

      expectError ErrorLocked $ exec conn "DROP TABLE foo"
      withConnShared $ \conn ->
        expectError ErrorBusy $ exec conn "DROP TABLE foo"

      Done <- step stmt
      2 <- columnCount stmt
      exec conn "DROP TABLE foo"

      -- Regular 'reset' throws away the error.  Make sure sqlite3_reset did
      -- not return an error because foo is now gone.  sqlite3_reset should
      -- only return an error if the most recent 'step' failed.
      Right () <- Direct.reset stmt

      -- But trying to 'step' again should fail.
      Left SQLError{sqlError = err} <- try $ step stmt
      assertBool "Step after table vanishes should fail with SQLITE_ERROR or SQLITE_SCHEMA"
                 (err == ErrorError ||  -- SQLite 3.7.13
                  err == ErrorSchema)   -- SQLite 3.6.22

  where
    expectError err io = do
      Left SQLError{sqlError = err'} <- try io
      assertEqual "testErrors: expectError" err err'

    foo123456 conn =
      exec conn "CREATE TABLE foo (a INT, b INT); \
                \INSERT INTO foo VALUES (1, 2); \
                \INSERT INTO foo VALUES (3, 4); \
                \INSERT INTO foo VALUES (5, 6)"

-- Make sure data stored in a table comes back as-is.
testIntegrity :: TestEnv -> Test
testIntegrity TestEnv{..} = TestCase $ do
  withConn $ \conn -> do
    exec conn "CREATE TABLE foo (i INT, f FLOAT, t TEXT, b BLOB, n TEXT)"
    withStmt conn "INSERT INTO foo VALUES (?, ?, ?, ?, ?)" $ \insert ->
      withStmt conn "SELECT * FROM foo" $ \select -> do
        let test = testWith (===)

            testWith f values = do
              exec conn "DELETE FROM foo"

              reset insert
              bind insert values
              Done <- step insert

              reset select
              Row <- step select
              values' <- columns select
              Done <- step select

              return $ f values values'

        True <- test [SQLInteger 0, SQLFloat 0.0, SQLText T.empty, SQLBlob B.empty, SQLNull]
        True <- test [SQLInteger minBound, SQLFloat (-1/0), SQLText "\0", SQLBlob (B8.pack "\0"), SQLNull]
        True <- test [SQLInteger maxBound, SQLFloat (1/0), SQLText "\1114111", SQLBlob ("\255"), SQLNull]

        -- SQLite3 turns NaN into SQLNull.
        True <- testWith (\_old new -> new === [SQLNull, SQLNull, SQLNull, SQLNull, SQLNull])
                [SQLNull, SQLFloat (0/0), SQLNull, SQLNull, SQLNull]

        return ()

testDecodeError :: TestEnv -> Test
testDecodeError TestEnv{..} = TestCase $ do
  withStmt conn "SELECT ?" $ \stmt -> do
    Right () <- Direct.bindText stmt 1 invalidUtf8
    Row <- step stmt
    Left (DecodeError "Database.SQLite3.columnText: Invalid UTF-8" _)
      <- try $ column stmt 0
    return ()

  -- Verify the assertion that SQLite3 does not validate UTF-8, by writing the
  -- data to a table on disk and reading it back.
  withConnShared $ \conn -> do
    exec conn "CREATE TABLE testDecodeError (a TEXT)"
    withStmt conn "INSERT INTO testDecodeError VALUES (?)" $ \stmt -> do
      Right () <- Direct.bindText stmt 1 invalidUtf8
      Done <- step stmt
      return ()
  withConnShared $ \conn -> do
    withStmt conn "SELECT * FROM testDecodeError" $ \stmt -> do
      Row <- step stmt
      TextColumn <- columnType stmt 0
      txt <- Direct.columnText stmt 0
      assertEqual "testDecodeError: Database altered our invalid UTF-8" invalidUtf8 txt
      Left (DecodeError "Database.SQLite3.columnText: Invalid UTF-8" _)
        <- try $ columnText stmt 0
      Done <- step stmt
      return ()

  where
    invalidUtf8 = Direct.Utf8 $ B.pack [0x80]

testMultiRowInsert :: TestEnv -> Test
testMultiRowInsert TestEnv{..} = TestCase $ do
  withConn $ \conn -> do
    exec conn "CREATE TABLE foo (a INT, b INT)"
    result <- try $ exec conn "INSERT INTO foo VALUES (1,2), (3,4)"
    case result of
      Left SQLError{sqlError = ErrorError} ->
        assertFailure "Installed SQLite3 does not support multi-row INSERT via the VALUES clause"
      Left e ->
        assertFailure $ show e
      Right () -> do
        -- Make sure multi-row insert actually worked
        withStmt conn "SELECT * FROM foo" $ \stmt -> do
          Row <- step stmt
          [SQLInteger 1, SQLInteger 2] <- columns stmt
          Row <- step stmt
          [SQLInteger 3, SQLInteger 4] <- columns stmt
          Done <- step stmt
          return ()


sharedDBPath :: Text
sharedDBPath = "dist/test/direct-sqlite-test-database.db"

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv cb =
    withConn $ \conn ->
        cb TestEnv
            { conn           = conn
            , withConn       = withConn
            , withConnShared = withConnPath sharedDBPath
            }
  where
    withConn = withConnPath ":memory:"
    withConnPath path cb = do
      conn <- open path
      r <- cb conn `onException` Direct.close conn
            -- If the callback throws an exception, try to close the DB.
            -- If closing fails (usually due to open 'Statement's),
            -- throw the original error, not the error produced by 'close'.
            -- Direct.close returns the error rather than throwing it.
      close conn
      return r

runTestGroup :: [TestEnv -> Test] -> IO Bool
runTestGroup tests = do
  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  return (cases == tried && errors == 0 && failures == 0)

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]

  T.putStrLn $ "Creating " `T.append` sharedDBPath
  handleJust (\e -> if isDoesNotExistError e
                       then Just ()
                       else Nothing)
             (\_ -> return ())
             (removeFile $ T.unpack sharedDBPath)
  open sharedDBPath >>= close

  ok <- runTestGroup regressionTests
  when (not ok) exitFailure

  -- Signal failure if feature tests fail.  I'd rather print a noisy warning
  -- instead, but cabal redirects test output to log files by default.
  ok <- runTestGroup featureTests
  when (not ok) exitFailure
