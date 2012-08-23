{-# LANGUAGE ScopedTypeVariables #-}

import Database.SQLite3

import Prelude hiding (catch)   -- Remove this import when GHC 7.6 is released,
                                -- as Prelude no longer exports catch.
import Control.Exception    (bracket, try)
import Control.Monad        (when)
import System.Directory
import System.Exit          (exitFailure)
import System.IO
import System.IO.Error      (isDoesNotExistError, isUserError)
import Test.HUnit

import qualified Control.Exception as E

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
    [ TestLabel "Exec"   . testExec
    , TestLabel "Simple" . testSimplest
    , TestLabel "Params" . testBind
    , TestLabel "Params" . testBindParamCounts
    , TestLabel "Params" . testBindParamName
    , TestLabel "Params" . testBindErrorValidation
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
    bracket (prepare conn "SELECT * FROM foo") finalize $ \stmt -> do
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
  E.handleJust (\e -> if isDoesNotExistError e
                         then Just ()
                         else Nothing)
               (\_ -> return ())
               (removeFile sharedDBPath)
  open sharedDBPath >>= close

  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure

