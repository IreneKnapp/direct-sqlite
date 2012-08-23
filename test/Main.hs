{-# LANGUAGE ScopedTypeVariables #-}

import Database.SQLite3

import Prelude hiding (catch)   -- Remove this import when GHC 7.6 is released,
                                -- as Prelude no longer exports catch.
import Control.Exception    (IOException, bracket)
import Control.Monad        (when)
import System.Directory
import System.Exit          (exitFailure)
import System.IO
import System.IO.Error      (isDoesNotExistError)
import Test.HUnit

import qualified Control.Exception as E

data TestEnv =
  TestEnv {
    conn :: Database
    -- ^ Database shared by all the tests
  , withConn :: forall a. (Database -> IO a) -> IO a
    -- ^ Bracket for spawning additional connections
  , withConnShared :: forall a. (Database -> IO a) -> IO a
    -- ^ Like 'withConn', but every invocation shares the same database.
  }

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Simple" . testSimplest
    , TestLabel "Params" . testBind
    , TestLabel "Params" . testBindParamCounts
    , TestLabel "Params" . testBindParamName
    , TestLabel "Params" . testBindErrorValidation
    ]

assertBindErrorCaught :: IO a -> Assertion
assertBindErrorCaught action = do
  E.catch (action >> return False) (\(_err :: IOException) -> return True) >>=
    assertBool "assertExceptionCaught"

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
  nParams <- bracket (prepare conn "SELECT $a") finalize bindParameterCount
  assertEqual "single $a" 1 nParams
  nParams <- bracket (prepare conn "SELECT (?1+?1+?1+?2+?3)") finalize bindParameterCount
  assertEqual "3 unique ?NNNs" 3 nParams
  nParams <- bracket (prepare conn "SELECT (?+?+?)") finalize bindParameterCount
  assertEqual "3 positional" 3 nParams

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
  bracket (prepare conn "SELECT ?") finalize (\stmt -> assertBindErrorCaught (testException1 stmt))
  bracket (prepare conn "SELECT ?") finalize (\stmt -> assertBindErrorCaught (testException2 stmt))
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

