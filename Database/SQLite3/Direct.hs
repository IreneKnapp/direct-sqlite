{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
-- |
-- This API is a slightly lower-level version of "Database.SQLite3".  Namely:
--
--  * It returns errors instead of throwing them.
--
--  * It only uses cheap conversions.  None of these bindings convert from
--    'String' or 'T.Text'.
module Database.SQLite3.Direct (
    -- * Types
    Database(..),
    Statement(..),
    SQLData(..),

    -- ** Results and errors
    Result,
    StepResult(..),
    Error(..),

    -- ** Special types
    Utf8(..),
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount(..),

    -- * Connection management
    open,
    close,
    errmsg,

    -- * Statement management
    prepare,
    step,
    reset,
    finalize,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    columnCount,

    -- * Binding values to a prepared statement
    bind,
    binds,
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindNull,

    -- * Reading the result row
    column,
    columns,
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,
) where

import Database.SQLite3.Bindings

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Unsafe     as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative  ((<$>))
import Data.ByteString      (ByteString)
import Data.String          (IsString(..))
import Data.Typeable
import Foreign
import Foreign.C

newtype Database = Database (Ptr CDatabase)
    deriving Show

newtype Statement = Statement (Ptr CStatement)
    deriving Show

data StepResult
    = Row
    | Done
    deriving (Eq, Show)

type Result a = Either Error a

data SQLData
    = SQLInteger    !Int64
    | SQLFloat      !Double
    | SQLText       !Utf8
    | SQLBlob       !ByteString
    | SQLNull
    deriving (Eq, Show, Typeable)

-- | A 'ByteString' containing UTF8-encoded text with no NUL characters.
newtype Utf8 = Utf8 ByteString
    deriving (Eq, Show)

instance IsString Utf8 where
    fromString = Utf8 . T.encodeUtf8 . T.pack

packUtf8 :: a -> (Utf8 -> a) -> CString -> IO a
packUtf8 n f cstr | cstr == nullPtr = return n
                  | otherwise       = f . Utf8 <$> BS.packCString cstr

packCStringLen :: CString -> CNumBytes -> IO ByteString
packCStringLen cstr len =
    BS.packCStringLen (cstr, fromIntegral len)

-- | Like 'unsafeUseAsCStringLen', but if the string is empty,
-- never pass the callback a null pointer.
unsafeUseAsCStringLenNoNull :: ByteString -> (CString -> CNumBytes -> IO a) -> IO a
unsafeUseAsCStringLenNoNull bs cb
    | BS.null bs = cb (intPtrToPtr 1) 0
    | otherwise  = BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
                       cb ptr (fromIntegral len)

-- Convert a 'CError' to a 'Result', in the common case where
-- SQLITE_OK signals success and anything else signals an error.
--
-- Note that SQLITE_OK == 0.
toResult :: a -> CError -> Result a
toResult a (CError 0) = Right a
toResult _ code       = Left $ decodeError code

-- Only perform the action if the 'CError' is SQLITE_OK.
toResultM :: Monad m => m a -> CError -> m (Result a)
toResultM m (CError 0) = m >>= return . Right
toResultM _ code       = return $ Left $ decodeError code

toStepResult :: CError -> Result StepResult
toStepResult code =
    case decodeError code of
        ErrorRow  -> Right Row
        ErrorDone -> Right Done
        err       -> Left err

------------------------------------------------------------------------

open :: Utf8 -> IO (Result Database)
open (Utf8 path) =
    BS.useAsCString path $ \path' ->
        alloca $ \database ->
            c_sqlite3_open path' database >>=
                toResultM (Database <$> peek database)

close :: Database -> IO (Result ())
close (Database db) =
    toResult () <$> c_sqlite3_close db

errmsg :: Database -> IO Utf8
errmsg (Database db) =
    c_sqlite3_errmsg db >>= packUtf8 (Utf8 BS.empty) id

prepare :: Database -> Utf8 -> IO (Result Statement)
prepare (Database db) (Utf8 sql) =
    BS.useAsCString sql $ \sql' ->
        alloca $ \statement ->
            c_sqlite3_prepare_v2 db sql' (-1) statement nullPtr >>=
                toResultM (Statement <$> peek statement)

step :: Statement -> IO (Result StepResult)
step (Statement stmt) =
    toStepResult <$> c_sqlite3_step stmt

reset :: Statement -> IO (Result ())
reset (Statement stmt) =
    toResult () <$> c_sqlite3_reset stmt

finalize :: Statement -> IO (Result ())
finalize (Statement stmt) =
    toResult () <$> c_sqlite3_finalize stmt

bindParameterCount :: Statement -> IO ParamIndex
bindParameterCount (Statement stmt) =
    c_sqlite3_bind_parameter_count stmt

bindParameterName :: Statement -> ParamIndex -> IO (Maybe Utf8)
bindParameterName (Statement stmt) idx =
    c_sqlite3_bind_parameter_name stmt idx >>=
        packUtf8 Nothing Just

columnCount :: Statement -> IO ColumnCount
columnCount (Statement stmt) =
    c_sqlite3_column_count stmt

bindInt64 :: Statement -> ParamIndex -> Int64 -> IO (Result ())
bindInt64 (Statement stmt) idx value =
    toResult () <$> c_sqlite3_bind_int64 stmt idx value

bindDouble :: Statement -> ParamIndex -> Double -> IO (Result ())
bindDouble (Statement stmt) idx value =
    toResult () <$> c_sqlite3_bind_double stmt idx value

bindText :: Statement -> ParamIndex -> Utf8 -> IO (Result ())
bindText (Statement stmt) idx (Utf8 value) =
    unsafeUseAsCStringLenNoNull value $ \ptr len ->
        toResult () <$>
            c_sqlite3_bind_text stmt idx ptr len c_SQLITE_TRANSIENT

bindBlob :: Statement -> ParamIndex -> ByteString -> IO (Result ())
bindBlob (Statement stmt) idx value =
    unsafeUseAsCStringLenNoNull value $ \ptr len ->
        toResult () <$>
            c_sqlite3_bind_blob stmt idx ptr len c_SQLITE_TRANSIENT

bindNull :: Statement -> ParamIndex -> IO (Result ())
bindNull (Statement stmt) idx =
    toResult () <$> c_sqlite3_bind_null stmt idx

columnType :: Statement -> ColumnIndex -> IO ColumnType
columnType (Statement stmt) idx =
    decodeColumnType <$> c_sqlite3_column_type stmt idx

columnInt64 :: Statement -> ColumnIndex -> IO Int64
columnInt64 (Statement stmt) idx =
    c_sqlite3_column_int64 stmt idx

columnDouble :: Statement -> ColumnIndex -> IO Double
columnDouble (Statement stmt) idx =
    c_sqlite3_column_double stmt idx

columnText :: Statement -> ColumnIndex -> IO Utf8
columnText (Statement stmt) idx = do
    ptr <- c_sqlite3_column_text stmt idx
    len <- c_sqlite3_column_bytes stmt idx
    Utf8 <$> packCStringLen ptr len

columnBlob :: Statement -> ColumnIndex -> IO ByteString
columnBlob (Statement stmt) idx = do
    ptr <- c_sqlite3_column_blob stmt idx
    len <- c_sqlite3_column_bytes stmt idx
    packCStringLen ptr len

bind :: Statement -> ParamIndex -> SQLData -> IO (Result ())
bind statement idx datum =
    case datum of
        SQLInteger v -> bindInt64  statement idx v
        SQLFloat   v -> bindDouble statement idx v
        SQLText    v -> bindText   statement idx v
        SQLBlob    v -> bindBlob   statement idx v
        SQLNull      -> bindNull   statement idx

column :: Statement -> ColumnIndex -> IO SQLData
column statement idx = do
    theType <- columnType statement idx
    case theType of
        IntegerColumn -> SQLInteger <$> columnInt64  statement idx
        FloatColumn   -> SQLFloat   <$> columnDouble statement idx
        TextColumn    -> SQLText    <$> columnText   statement idx
        BlobColumn    -> SQLBlob    <$> columnBlob   statement idx
        NullColumn    -> return SQLNull

binds :: Statement -> [SQLData] -> IO (Result ())
binds = undefined

columns :: Statement -> IO [SQLData]
columns = undefined
