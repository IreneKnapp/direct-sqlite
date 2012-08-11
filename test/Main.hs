
import Control.Exception (bracket)
import Control.Monad     (when)
import System.Exit       (exitFailure)
import System.IO
import Test.HUnit

import Database.SQLite3

data TestEnv =
  TestEnv {
    conn :: Database
    -- ^ Database shared by all the tests
  , withConn :: forall a. (Database -> IO a) -> IO a
    -- ^ Bracket for spawning additional connections
  }

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Simple" . testSimplest
    ]

-- Simplest SELECT
testSimplest :: TestEnv -> Test
testSimplest TestEnv{..} = TestCase $ do
  stmt <- prepare conn "SELECT 1+1"
  Row <- step stmt
  res <- column stmt 0
  Done <- step stmt
  finalize stmt
  assertEqual "1+1" (SQLInteger 2) res

-- | Action for connecting to the database that will be used for
-- testing.
--
-- Note that some tests, such as Notify, use multiple connections, and
-- assume that 'testConnect' connects to the same database every time
-- it is called.
testConnect :: IO Database
testConnect = open ":memory:"

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv cb =
    withConn $ \conn ->
        cb TestEnv
            { conn     = conn
            , withConn = withConn
            }
  where
    withConn = bracket testConnect close

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure
