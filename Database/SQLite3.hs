{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.SQLite3 (
    -- * Connection management
    open,
    close,

    -- * Simple query execution
    -- | <http://sqlite.org/c3ref/exec.html>
    exec,
    execPrint,
    execWithCallback,
    ExecCallback,

    -- * Statement management
    prepare,
    prepareUtf8,
    step,
    reset,
    finalize,
    clearBindings,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    columnCount,
    columnName,

    -- * Binding values to a prepared statement
    -- | <http://www.sqlite.org/c3ref/bind_blob.html>
    bindSQLData,
    bind,
    bindInt,
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindNull,

    -- * Reading the result row
    -- | <http://www.sqlite.org/c3ref/column_blob.html>
    --
    -- Warning: 'column' and 'columns' will throw a 'DecodeError' if any @TEXT@
    -- datum contains invalid UTF-8.
    column,
    columns,
    typedColumns,
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * Result statistics
    lastInsertRowId,
    changes,

    -- * Interrupting a long-running query
    interrupt,
    interruptibly,

    -- * Types
    Database,
    Statement,
    SQLData(..),
    SQLError(..),
    ColumnType(..),

    -- ** Results and errors
    StepResult(..),
    Error(..),

    -- ** Special integers
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,
) where

import Database.SQLite3.Direct
    ( Database
    , Statement
    , ColumnType(..)
    , StepResult(..)
    , Error(..)
    , ParamIndex(..)
    , ColumnIndex(..)
    , ColumnCount
    , Utf8(..)

    -- Re-exported from Database.SQLite3.Direct without modification.
    -- Note that if this module were in another package, source links would not
    -- be generated for these functions.
    , clearBindings
    , bindParameterCount
    , columnCount
    , columnType
    , columnBlob
    , columnInt64
    , columnDouble
    , lastInsertRowId
    , changes
    , interrupt
    )

import qualified Database.SQLite3.Direct as Direct

import Prelude hiding (error)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative  ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad        (when, zipWithM, zipWithM_)
import Data.ByteString      (ByteString)
import Data.Int             (Int64)
import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (UnicodeException(..), lenientDecode)
import Data.Typeable

data SQLData
    = SQLInteger    !Int64
    | SQLFloat      !Double
    | SQLText       !Text
    | SQLBlob       !ByteString
    | SQLNull
    deriving (Eq, Show, Typeable)

-- | Exception thrown when SQLite3 reports an error.
--
-- direct-sqlite may throw other types of exceptions if you misuse the API.
data SQLError = SQLError
    { sqlError          :: !Error
        -- ^ Error code returned by API call
    , sqlErrorDetails   :: Text
        -- ^ Text describing the error
    , sqlErrorContext   :: Text
        -- ^ Indicates what action produced this error,
        --   e.g. @exec \"SELECT * FROM foo\"@
    }
    deriving Typeable

-- NB: SQLError is lazy in 'sqlErrorDetails' and 'sqlErrorContext',
-- to defer message construction in the case where a user catches and
-- immediately handles the error.


instance Show SQLError where
    show SQLError{ sqlError        = code
                 , sqlErrorDetails = details
                 , sqlErrorContext = context
                 }
         = T.unpack $ T.concat
         [ "SQLite3 returned "
         , T.pack $ show code
         , " while attempting to perform "
         , context
         , ": "
         , details
         ]

instance Exception SQLError

-- | Like 'decodeUtf8', but substitute a custom error message if
-- decoding fails.
fromUtf8 :: String -> Utf8 -> IO Text
fromUtf8 desc utf8 = evaluate $ fromUtf8' desc utf8

fromUtf8' :: String -> Utf8 -> Text
fromUtf8' desc (Utf8 bs) =
    decodeUtf8With (\_ c -> throw (DecodeError desc c)) bs

toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . encodeUtf8

data DetailSource
    = DetailDatabase    Database
    | DetailStatement   Statement
    | DetailMessage     Utf8

renderDetailSource :: DetailSource -> IO Utf8
renderDetailSource src = case src of
    DetailDatabase db ->
        Direct.errmsg db
    DetailStatement stmt -> do
        db <- Direct.getStatementDatabase stmt
        Direct.errmsg db
    DetailMessage msg ->
        return msg

throwSQLError :: DetailSource -> Text -> Error -> IO a
throwSQLError detailSource context error = do
    Utf8 details <- renderDetailSource detailSource
    throwIO SQLError
        { sqlError        = error
        , sqlErrorDetails = decodeUtf8With lenientDecode details
        , sqlErrorContext = context
        }

checkError :: DetailSource -> Text -> Either Error a -> IO a
checkError ds fn = either (throwSQLError ds fn) return

checkErrorMsg :: Text -> Either (Error, Utf8) a -> IO a
checkErrorMsg fn result = case result of
    Left (err, msg) -> throwSQLError (DetailMessage msg) fn err
    Right a         -> return a

appendShow :: Show a => Text -> a -> Text
appendShow txt a = txt `T.append` (T.pack . show) a


-- | <http://www.sqlite.org/c3ref/open.html>
open :: Text -> IO Database
open path =
    Direct.open (toUtf8 path)
        >>= checkErrorMsg ("open " `appendShow` path)

-- | <http://www.sqlite.org/c3ref/close.html>
close :: Database -> IO ()
close db =
    Direct.close db >>= checkError (DetailDatabase db) "close"

-- | Make it possible to interrupt the given database operation with an
-- asynchronous exception.  This only works if the program is compiled with
-- base >= 4.3 and @-threaded@.
--
-- It works by running the callback in a forked thread.  If interrupted,
-- it uses 'interrupt' to try to stop the operation.
interruptibly :: Database -> IO a -> IO a
#if MIN_VERSION_base(4,3,0)
interruptibly db io
  | rtsSupportsBoundThreads =
      mask $ \restore -> do
          mv <- newEmptyMVar
          tid <- forkIO $ try' (restore io) >>= putMVar mv

          let interruptAndWait =
                  -- Don't let a second exception interrupt us.  Otherwise,
                  -- the operation will dangle in the background, which could
                  -- be really bad if it uses locally-allocated resources.
                  uninterruptibleMask_ $ do
                      -- Tell SQLite3 to interrupt the current query.
                      interrupt db

                      -- Interrupt the thread in case it's blocked for some
                      -- other reason.
                      --
                      -- NOTE: killThread blocks until the exception is delivered.
                      -- That's fine, since we're going to wait for the thread
                      -- to finish anyway.
                      killThread tid

                      -- Wait for the forked thread to finish.
                      _ <- takeMVar mv
                      return ()

          e <- takeMVar mv `onException` interruptAndWait
          either throwIO return e
  | otherwise = io
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try
#else
interruptibly _db io = io
#endif

-- | Execute zero or more SQL statements delimited by semicolons.
exec :: Database -> Text -> IO ()
exec db sql =
    Direct.exec db (toUtf8 sql)
        >>= checkErrorMsg ("exec " `appendShow` sql)

-- | Like 'exec', but print result rows to 'System.IO.stdout'.
--
-- This is mainly for convenience when experimenting in GHCi.
-- The output format may change in the future.
execPrint :: Database -> Text -> IO ()
execPrint !db !sql =
    interruptibly db $
    execWithCallback db sql $ \_count _colnames -> T.putStrLn . showValues
  where
    -- This mimics sqlite3's default output mode.  It displays a NULL and an
    -- empty string identically.
    showValues = T.intercalate "|" . map (fromMaybe "")

-- | Like 'exec', but invoke the callback for each result row.
execWithCallback :: Database -> Text -> ExecCallback -> IO ()
execWithCallback db sql cb =
    Direct.execWithCallback db (toUtf8 sql) cb'
        >>= checkErrorMsg ("execWithCallback " `appendShow` sql)
  where
    -- We want 'names' computed once and shared with every call.
    cb' count namesUtf8 =
       let names = map fromUtf8'' namesUtf8
           {-# NOINLINE names #-}
        in cb count names . map (fmap fromUtf8'')

    fromUtf8'' = fromUtf8' "Database.SQLite3.execWithCallback: Invalid UTF-8"

type ExecCallback
     = ColumnCount    -- ^ Number of columns, which is the number of items in
                      --   the following lists.  This will be the same for
                      --   every row.
    -> [Text]         -- ^ List of column names.  This will be the same
                      --   for every row.
    -> [Maybe Text]   -- ^ List of column values, as returned by 'columnText'.
    -> IO ()

-- | <http://www.sqlite.org/c3ref/prepare.html>
--
-- Unlike 'exec', 'prepare' only executes the first statement, and ignores
-- subsequent statements.
--
-- If the query string contains no SQL statements, this 'fail's.
prepare :: Database -> Text -> IO Statement
prepare db sql = prepareUtf8 db (toUtf8 sql)
        
-- | <http://www.sqlite.org/c3ref/prepare.html>
--
-- It can help to avoid redundant Utf8 to Text conversion if you already
-- have Utf8 
--
-- If the query string contains no SQL statements, this 'fail's.
prepareUtf8 :: Database -> Utf8 -> IO Statement
prepareUtf8 db sql = do
    m <- Direct.prepare db sql
            >>= checkError (DetailDatabase db) ("prepare " `appendShow` sql)
    case m of
        Nothing   -> fail "Direct.SQLite3.prepare: empty query string"
        Just stmt -> return stmt

-- | <http://www.sqlite.org/c3ref/step.html>
step :: Statement -> IO StepResult
step statement =
    Direct.step statement >>= checkError (DetailStatement statement) "step"

-- Note: sqlite3_reset and sqlite3_finalize return an error code if the most
-- recent sqlite3_step indicated an error.  I think these are the only times
-- these functions return an error (barring memory corruption and misuse of the API).
--
-- We don't replicate that behavior here.  Instead, 'reset' and 'finalize'
-- discard the error.  Otherwise, we would get "double jeopardy".
-- For example:
--
--  ok <- try $ step stmt :: IO (Either SQLError StepResult)
--  finalize stmt
--
-- If 'finalize' threw its error, it would throw the exception the user was
-- trying to catch.
--
-- 'reset' and 'finalize' might return a different error than the step that
-- failed, leading to more cryptic error messages [1].  But we're not
-- completely sure about this.
--
--  [1]: https://github.com/yesodweb/persistent/issues/92#issuecomment-7806421

-- | <http://www.sqlite.org/c3ref/reset.html>
--
-- Note that in the C API, @sqlite3_reset@ returns an error code if the most
-- recent @sqlite3_step@ indicated an error.  We do not replicate that behavior
-- here.  'reset' never throws an exception.
reset :: Statement -> IO ()
reset statement = do
    _ <- Direct.reset statement
    return ()

-- | <http://www.sqlite.org/c3ref/finalize.html>
--
-- Like 'reset', 'finalize' never throws an exception.
finalize :: Statement -> IO ()
finalize statement = do
    _ <- Direct.finalize statement
    return ()


-- | <http://www.sqlite.org/c3ref/bind_parameter_name.html>
--
-- Return the N-th SQL parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the parameter index starts at 1, not 0.
bindParameterName :: Statement -> ParamIndex -> IO (Maybe Text)
bindParameterName stmt idx = do
    m <- Direct.bindParameterName stmt idx
    case m of
        Nothing   -> return Nothing
        Just name -> Just <$> fromUtf8 desc name
  where
    desc = "Database.SQLite3.bindParameterName: Invalid UTF-8"

-- | <http://www.sqlite.org/c3ref/column_name.html>
--
-- Return the name of a result column.  If the column index is out of range,
-- return 'Nothing'.
columnName :: Statement -> ColumnIndex -> IO (Maybe Text)
columnName stmt idx = do
    m <- Direct.columnName stmt idx
    case m of
        Just name -> Just <$> fromUtf8 desc name
        Nothing -> do
            -- sqlite3_column_name only returns NULL if memory allocation fails
            -- or if the column index is out of range.
            count <- Direct.columnCount stmt
            if idx >= 0 && idx < count
                then throwIO outOfMemory
                else return Nothing
  where
    desc = "Database.SQLite3.columnName: Invalid UTF-8"
    outOfMemory = SQLError
        { sqlError        = ErrorNoMemory
        , sqlErrorDetails = "out of memory (sqlite3_column_name returned NULL)"
        , sqlErrorContext = "column name"
        }

bindBlob :: Statement -> ParamIndex -> ByteString -> IO ()
bindBlob statement parameterIndex byteString =
    Direct.bindBlob statement parameterIndex byteString
        >>= checkError (DetailStatement statement) "bind blob"

bindDouble :: Statement -> ParamIndex -> Double -> IO ()
bindDouble statement parameterIndex datum =
    Direct.bindDouble statement parameterIndex datum
        >>= checkError (DetailStatement statement) "bind double"

bindInt :: Statement -> ParamIndex -> Int -> IO ()
bindInt statement parameterIndex datum =
    Direct.bindInt64 statement
                     parameterIndex
                     (fromIntegral datum)
        >>= checkError (DetailStatement statement) "bind int"

bindInt64 :: Statement -> ParamIndex -> Int64 -> IO ()
bindInt64 statement parameterIndex datum =
    Direct.bindInt64 statement parameterIndex datum
        >>= checkError (DetailStatement statement) "bind int64"

bindNull :: Statement -> ParamIndex -> IO ()
bindNull statement parameterIndex =
    Direct.bindNull statement parameterIndex
        >>= checkError (DetailStatement statement) "bind null"

bindText :: Statement -> ParamIndex -> Text -> IO ()
bindText statement parameterIndex text =
    Direct.bindText statement parameterIndex (toUtf8 text)
        >>= checkError (DetailStatement statement) "bind text"

-- | If the index is not between 1 and 'bindParameterCount' inclusive, this
-- fails with 'ErrorRange'.  Otherwise, it succeeds, even if the query skips
-- this index by using numbered parameters.
--
-- Example:
--
-- >> stmt <- prepare conn "SELECT ?1, ?3, ?5"
-- >> bindSQLData stmt 1 (SQLInteger 1)
-- >> bindSQLData stmt 2 (SQLInteger 2)
-- >> bindSQLData stmt 6 (SQLInteger 6)
-- >*** Exception: SQLite3 returned ErrorRange while attempting to perform bind int64.
-- >> step stmt >> columns stmt
-- >[SQLInteger 1,SQLNull,SQLNull]
bindSQLData :: Statement -> ParamIndex -> SQLData -> IO ()
bindSQLData statement idx datum =
    case datum of
        SQLInteger v -> bindInt64  statement idx v
        SQLFloat   v -> bindDouble statement idx v
        SQLText    v -> bindText   statement idx v
        SQLBlob    v -> bindBlob   statement idx v
        SQLNull      -> bindNull   statement idx

-- | Convenience function for binding values to all parameters.  This will
-- 'fail' if the list has the wrong number of parameters.
bind :: Statement -> [SQLData] -> IO ()
bind statement sqlData = do
    ParamIndex nParams <- bindParameterCount statement
    when (nParams /= length sqlData) $
        fail ("mismatched parameter count for bind.  Prepared statement "++
              "needs "++ show nParams ++ ", " ++ show (length sqlData) ++" given")
    zipWithM_ (bindSQLData statement) [1..] sqlData

-- |
-- This will throw a 'DecodeError' if the datum contains invalid UTF-8.
-- If this behavior is undesirable, you can use 'Direct.columnText' from
-- "Database.SQLite3.Direct", which does not perform conversion to 'Text'.
columnText :: Statement -> ColumnIndex -> IO Text
columnText statement columnIndex =
    Direct.columnText statement columnIndex
        >>= fromUtf8 "Database.SQLite3.columnText: Invalid UTF-8"

column :: Statement -> ColumnIndex -> IO SQLData
column statement idx = do
    theType <- columnType statement idx
    typedColumn theType statement idx

columns :: Statement -> IO [SQLData]
columns statement = do
    count <- columnCount statement
    mapM (column statement) [0..count-1]

typedColumn :: ColumnType -> Statement -> ColumnIndex -> IO SQLData
typedColumn theType statement idx = case theType of
    IntegerColumn -> SQLInteger <$> columnInt64  statement idx
    FloatColumn   -> SQLFloat   <$> columnDouble statement idx
    TextColumn    -> SQLText    <$> columnText   statement idx
    BlobColumn    -> SQLBlob    <$> columnBlob   statement idx
    NullColumn    -> return SQLNull

-- |
-- This avoids extra API calls using the list of column types.
-- If passed types do not correspond to the actual types, the values will be 
-- converted according to the rules at <http://www.sqlite.org/c3ref/column_blob.html>.
-- If the list contains more items that number of columns, the result is undefined.
typedColumns :: Statement -> [Maybe ColumnType] -> IO [SQLData]
typedColumns statement = zipWithM f [0..] where
    f idx theType = case theType of
        Nothing -> column statement idx
        Just t  -> typedColumn t statement idx
