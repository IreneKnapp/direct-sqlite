{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite3 (
    -- * Connection management
    open,
    close,
    errmsg,

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
    bind,
    binds,
    bindInt,
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindNull,

    -- * Reading the result row
    -- | <http://www.sqlite.org/c3ref/column_blob.html>
    column,
    columns,
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * Types
    Database(..),
    Statement(..),
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
    )

import qualified Database.SQLite3.Direct as Direct

import Prelude hiding (error)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative  ((<$>))
import Control.Exception    (Exception, throwIO)
import Control.Monad        (when, zipWithM_)
import Data.Int             (Int64)
import Data.String          (fromString)
import Data.Typeable

data SQLData
    = SQLInteger    !Int64
    | SQLFloat      !Double
    | SQLText       !T.Text
    | SQLBlob       !BS.ByteString
    | SQLNull
    deriving (Eq, Show, Typeable)

-- | Exception thrown when SQLite3 reports an error.
--
-- direct-sqlite may throw other types of exceptions if you misuse the API.
data SQLError = SQLError
    { sqlError          :: !Error
        -- ^ Error code returned by API call
    , sqlErrorDetails   :: Maybe String
        -- ^ Text describing the error, if available
    , sqlErrorContext   :: String
        -- ^ Indicates what action produced this error,
        --   e.g. @exec \"SELECT * FROM foo\"@
    }
    deriving Typeable

instance Show SQLError where
    show SQLError{ sqlError        = code
                 , sqlErrorDetails = details
                 , sqlErrorContext = context
                 }
         = "SQLite3 returned " ++ show code
        ++ " while attempting to perform " ++ context
        ++ case details of
               Nothing -> "."
               Just s  -> ": " ++ s

instance Exception SQLError

fromUtf8 :: Utf8 -> String
fromUtf8 (Utf8 bs) = (T.unpack . T.decodeUtf8) bs

toUtf8 :: String -> Utf8
toUtf8 = fromString

errmsg :: Database -> IO String
errmsg db = fromUtf8 <$> Direct.errmsg db

data DetailSource
    = DetailNone
    | DetailDatabase Database
    | DetailMessage  Utf8

renderDetailSource :: DetailSource -> IO (Maybe String)
renderDetailSource src = case src of
    DetailNone ->
        return Nothing
    DetailDatabase db -> do
        details <- errmsg db
        return $ Just details
    DetailMessage utf8 ->
        return $ Just $ fromUtf8 utf8

throwSQLError :: DetailSource -> String -> Error -> IO a
throwSQLError detailSource context error = do
    details <- renderDetailSource detailSource
    throwIO SQLError
        { sqlError        = error
        , sqlErrorDetails = details
        , sqlErrorContext = context
        }

checkError :: DetailSource -> String -> Either Error a -> IO a
checkError ds fn = either (throwSQLError ds fn) return

open :: String -> IO Database
open path = do
    Direct.open (toUtf8 path)
        >>= checkError DetailNone ("open " ++ show path)

close :: Database -> IO ()
close db =
    Direct.close db >>= checkError (DetailDatabase db) "close"

exec :: Database -> String -> IO ()
exec db sql =
    Direct.exec db (toUtf8 sql)
        >>= checkErrorMsg ("exec " ++ show sql)
  where
    checkErrorMsg fn result = case result of
        Left (err, msg) -> throwSQLError (DetailMessage msg) fn err
        Right ()        -> return ()

prepare :: Database -> String -> IO Statement
prepare db sql =
    Direct.prepare db (toUtf8 sql) >>=
        checkError (DetailDatabase db) ("prepare " ++ (show sql))

step :: Statement -> IO StepResult
step statement =
    Direct.step statement >>= checkError DetailNone "step"

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

reset :: Statement -> IO ()
reset statement = do
    _ <- Direct.reset statement
    return ()

finalize :: Statement -> IO ()
finalize statement = do
    _ <- Direct.finalize statement
    return ()


-- | This returns the index of the largest (rightmost) parameter.  Note that
-- this is not necessarily the number of parameters.  If numbered parameters
-- like @?5@ are used, there may be gaps in the list.
--
-- See 'ParamIndex' for more information.
bindParameterCount :: Statement -> IO ParamIndex
bindParameterCount = Direct.bindParameterCount

-- | Return the N-th SQL parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the parameter index starts at 1, not 0.
bindParameterName :: Statement -> ParamIndex -> IO (Maybe String)
bindParameterName stmt idx =
    fmap fromUtf8 <$>
    Direct.bindParameterName stmt idx

bindBlob :: Statement -> ParamIndex -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString =
    Direct.bindBlob statement parameterIndex byteString
        >>= checkError DetailNone "bind blob"

bindDouble :: Statement -> ParamIndex -> Double -> IO ()
bindDouble statement parameterIndex datum =
    Direct.bindDouble statement parameterIndex datum
        >>= checkError DetailNone "bind double"

bindInt :: Statement -> ParamIndex -> Int -> IO ()
bindInt statement parameterIndex datum =
    Direct.bindInt64 statement
                     parameterIndex
                     (fromIntegral datum)
        >>= checkError DetailNone "bind int"

bindInt64 :: Statement -> ParamIndex -> Int64 -> IO ()
bindInt64 statement parameterIndex datum =
    Direct.bindInt64 statement parameterIndex datum
        >>= checkError DetailNone "bind int64"

bindNull :: Statement -> ParamIndex -> IO ()
bindNull statement parameterIndex =
    Direct.bindNull statement parameterIndex
        >>= checkError DetailNone "bind null"

bindText :: Statement -> ParamIndex -> T.Text -> IO ()
bindText statement parameterIndex text =
    Direct.bindText statement parameterIndex (Utf8 $ T.encodeUtf8 text)
        >>= checkError DetailNone "bind text"

-- | If the index is not between 1 and 'bindParameterCount' inclusive, this
-- fails with 'ErrorRange'.  Otherwise, it succeeds, even if the query skips
-- this index by using numbered parameters.
--
-- Example:
--
-- >> stmt <- prepare conn "SELECT ?1, ?3, ?5"
-- >> bind stmt 1 (SQLInteger 1)
-- >> bind stmt 2 (SQLInteger 2)
-- >> bind stmt 6 (SQLInteger 6)
-- >TODO
-- >> step stmt >> columns stmt
-- >[SQLInteger 1,SQLNull,SQLNull]
bind :: Statement -> ParamIndex -> SQLData -> IO ()
bind statement idx datum =
    case datum of
        SQLInteger v -> bindInt64  statement idx v
        SQLFloat   v -> bindDouble statement idx v
        SQLText    v -> bindText   statement idx v
        SQLBlob    v -> bindBlob   statement idx v
        SQLNull      -> bindNull   statement idx

binds :: Statement -> [SQLData] -> IO ()
binds statement sqlData = do
    nParams <- fromIntegral <$> bindParameterCount statement
    when (nParams /= length sqlData) $
        fail ("mismatched parameter count for bind.  Prepared statement "++
              "needs "++ show nParams ++ ", " ++ show (length sqlData) ++" given")
    zipWithM_ (bind statement) [1..] sqlData

columnType :: Statement -> ColumnIndex -> IO ColumnType
columnType = Direct.columnType

columnBlob :: Statement -> ColumnIndex -> IO BS.ByteString
columnBlob = Direct.columnBlob

columnInt64 :: Statement -> ColumnIndex -> IO Int64
columnInt64 = Direct.columnInt64

columnDouble :: Statement -> ColumnIndex -> IO Double
columnDouble = Direct.columnDouble

columnText :: Statement -> ColumnIndex -> IO T.Text
columnText statement columnIndex = do
    Utf8 bs <- Direct.columnText statement columnIndex
    return $! T.decodeUtf8 bs

columnCount :: Statement -> IO ColumnCount
columnCount = Direct.columnCount

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
