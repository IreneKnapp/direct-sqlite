{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Database.SQLite3.Direct (
    -- * Types
    Database(..),
    Statement(..),
    SQLData(..),

    -- ** Results and errors
    Result(..),
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative  ((<$>))
import Control.Monad
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

data Result a
    = Okay           a
    | Error         !Error
    | UnknownError  !Int
    deriving (Eq, Show)

data SQLData
    = SQLInteger    !Int64
    | SQLFloat      !Double
    | SQLText       !Utf8
    | SQLBlob       !ByteString
    | SQLNull
    deriving (Eq, Show, Typeable)

-- | A 'ByteString' containing UTF8-encoded text.
newtype Utf8 = Utf8 ByteString
    deriving (Eq, Show)

instance IsString Utf8 where
    fromString = Utf8 . T.encodeUtf8 . T.pack

packUtf8 :: CString -> IO Utf8
packUtf8 cstr = Utf8 <$> BS.packCString cstr

errorResult :: CError -> Result a
errorResult code@(CError n) =
    case decodeError code of
        Just err -> Error err
        Nothing  -> UnknownError (fromIntegral n)

-- Convert a 'CError' to a 'Result', in the common case where
-- SQLITE_OK signals success and anything else signals an error.
--
-- Note that SQLITE_OK == 0.
toResult :: a -> CError -> Result a
toResult a (CError 0) = Okay a
toResult _ code       = errorResult code

-- Only perform the action if the 'CError' is SQLITE_OK.
toResultM :: Monad m => m a -> CError -> m (Result a)
toResultM m (CError 0) = m >>= return . Okay
toResultM _ code       = return (errorResult code)

toStepResult :: CError -> Result StepResult
toStepResult code@(CError n) =
    case decodeError code of
        Just ErrorRow  -> Okay Row
        Just ErrorDone -> Okay Done
        Just err       -> Error err
        Nothing        -> UnknownError (fromIntegral n)

toColumnType :: CColumnType -> Either Int ColumnType
toColumnType code@(CColumnType n) =
    case decodeColumnType code of
        Just t  -> Right t
        Nothing -> Left (fromIntegral n)

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
    c_sqlite3_errmsg db >>= packUtf8

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
bindParameterName (Statement stmt) idx = do
    cstr <- c_sqlite3_bind_parameter_name stmt idx
    if cstr == nullPtr
        then return Nothing
        else Just <$> packUtf8 cstr

columnCount :: Statement -> IO ColumnCount
columnCount (Statement stmt) =
    c_sqlite3_column_count stmt

bindInt64 :: Statement -> ParamIndex -> Int64 -> IO (Result ())
bindInt64 (Statement stmt) idx value =
    toResult () <$> c_sqlite3_bind_int64 stmt idx value

bindDouble :: Statement -> ParamIndex -> Double -> IO (Result ())
bindDouble = undefined

bindText :: Statement -> ParamIndex -> Utf8 -> IO (Result ())
bindText = undefined

bindBlob :: Statement -> ParamIndex -> ByteString -> IO (Result ())
bindBlob = undefined

bindNull :: Statement -> ParamIndex -> IO (Result ())
bindNull = undefined


columnType :: Statement -> ColumnIndex -> IO ColumnType
columnType = undefined

columnInt64 :: Statement -> ColumnIndex -> IO Int64
columnInt64 = undefined

columnDouble :: Statement -> ColumnIndex -> IO Double
columnDouble = undefined

columnText :: Statement -> ColumnIndex -> IO Utf8
columnText = undefined

columnBlob :: Statement -> ColumnIndex -> IO ByteString
columnBlob = undefined


bind :: Statement -> ParamIndex -> SQLData -> IO (Result ())
bind = undefined

column :: Statement -> ColumnIndex -> IO SQLData
column = undefined

binds :: Statement -> [SQLData] -> IO (Result ())
binds = undefined

columns :: Statement -> IO [SQLData]
columns = undefined
