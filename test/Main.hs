{-# LANGUAGE ScopedTypeVariables #-}

import Database.SQLite3

import Prelude hiding (catch)   -- Remove this import when GHC 7.6 is released,
                                -- as Prelude no longer exports catch.
import Control.Exception    (bracket, handleJust, try)
import Control.Monad        (forM_, when)
import System.Directory
import System.Exit          (exitFailure)
import System.IO
import System.IO.Error      (isDoesNotExistError, isUserError)
import Test.HUnit

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

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Exec"          . testExec
    , TestLabel "Simple"        . testSimplest
    , TestLabel "Prepare"       . testPrepare
    , TestLabel "CloseBusy"     . testCloseBusy
    , TestLabel "Params"        . testBind
    , TestLabel "Params"        . testBindParamCounts
    , TestLabel "Params"        . testBindParamName
    , TestLabel "Params"        . testBindErrorValidation
    , TestLabel "Errors"        . testErrors
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

withStmt :: Database -> String -> (Statement -> IO a) -> IO a
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

-- Testing for specific error codes:
--
--  * ErrorConstraint
--
--  * ErrorRange
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

  where
    expectError err io = do
      Left SQLError{sqlError = err'} <- try io
      assertBool "testErrors: expectError" (err == err')


sharedDBPath :: String
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
    withConn          = withConnPath ":memory:"
    withConnPath path = bracket (open path) close

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]

  putStrLn $ "Creating " ++ sharedDBPath
  handleJust (\e -> if isDoesNotExistError e
                       then Just ()
                       else Nothing)
             (\_ -> return ())
             (removeFile sharedDBPath)
  open sharedDBPath >>= close

  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure

