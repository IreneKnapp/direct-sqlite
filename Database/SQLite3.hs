{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.SQLite3 (
    -- * Connection management
    open,
    close,

    -- * Simple query execution
    -- | <http://sqlite.org/c3ref/exec.html>
    exec,

    -- * Statement management
    prepare,
    step,
    reset,
    finalize,
    clearBindings,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    columnCount,

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
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * Interrupting a long-running query
    interrupt,

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
    , interrupt
    , clearBindings
    , bindParameterCount
    , columnCount
    , columnType
    , columnBlob
    , columnInt64
    , columnDouble
    )

import qualified Database.SQLite3.Direct as Direct

import Prelude hiding (error)
import qualified Data.Text as T
import Control.Applicative  ((<$>))
import Control.Exception    (Exception, evaluate, throw, throwIO)
import Control.Monad        (when, zipWithM_)
import Data.ByteString      (ByteString)
import Data.Int             (Int64)
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

fromUtf8 :: String -> Utf8 -> IO Text
fromUtf8 desc (Utf8 bs) =
    evaluate $ decodeUtf8With (\_ c -> throw (DecodeError desc c)) bs

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

-- | Execute zero or more SQL statements delimited by semicolons.
exec :: Database -> Text -> IO ()
exec db sql =
    Direct.exec db (toUtf8 sql)
        >>= checkErrorMsg ("exec " `appendShow` sql)

-- | <http://www.sqlite.org/c3ref/prepare.html>
--
-- Unlike 'exec', 'prepare' only executes the first statement, and ignores
-- subsequent statements.
--
-- If the query string contains no SQL statements, this 'fail's.
prepare :: Database -> Text -> IO Statement
prepare db sql = do
    m <- Direct.prepare db (toUtf8 sql)
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
    nParams <- fromIntegral <$> bindParameterCount statement
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
    case theType of
        IntegerColumn -> SQLInteger <$> columnInt64  statement idx
        FloatColumn   -> SQLFloat   <$> columnDouble statement idx
        TextColumn    -> SQLText    <$> columnText   statement idx
        BlobColumn    -> SQLBlob    <$> columnBlob   statement idx
        NullColumn    -> return SQLNull

columns :: Statement -> IO [SQLData]
columns statement = do
    count <- columnCount statement
    mapM (column statement) [0..count-1]
