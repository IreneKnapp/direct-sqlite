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

errorResult :: CError -> Result a
errorResult code@(CError n) =
    case decodeError code of
        Just err -> Error err
        Nothing  -> UnknownError (fromIntegral n)

-- Convert a 'CError' to a 'Result', in the common case where
-- SQLITE_OK signals success and anything else signals an error.
--
-- Note that SQLITE_OK == 0.
toResult :: CError -> a -> Result a
toResult (CError 0) a = Okay a
toResult code       _ = errorResult code

-- Only perform the action if the 'CError' is SQLITE_OK.
toResultM :: Monad m => CError -> m a -> m (Result a)
toResultM (CError 0) m = m >>= return . Okay
toResultM code       _ = return (errorResult code)

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
        alloca $ \database -> do
            rc <- c_sqlite3_open path' database
            toResultM rc (Database <$> peek database)

close :: Database -> IO ()
close = undefined

errmsg :: Database -> IO String
errmsg = undefined


prepare :: Database -> String -> IO Statement
prepare = undefined

step :: Statement -> IO StepResult
step = undefined

reset :: Statement -> IO ()
reset = undefined

finalize :: Statement -> IO ()
finalize = undefined


bindParameterCount :: Statement -> IO Int
bindParameterCount = undefined

bindParameterName :: Statement -> Int -> IO (Maybe String)
bindParameterName = undefined

columnCount :: Statement -> IO Int
columnCount = undefined


bind :: Statement -> ParamIndex -> SQLData -> IO ()
bind = undefined

binds :: Statement -> [SQLData] -> IO ()
binds = undefined

bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 = undefined

bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble = undefined

bindText :: Statement -> Int -> Utf8 -> IO ()
bindText = undefined

bindBlob :: Statement -> Int -> ByteString -> IO ()
bindBlob = undefined

bindNull :: Statement -> Int -> IO ()
bindNull = undefined


column :: Statement -> Int -> IO SQLData
column = undefined

columns :: Statement -> IO [SQLData]
columns = undefined

columnType :: Statement -> Int -> IO ColumnType
columnType = undefined

columnInt64 :: Statement -> Int -> IO Int64
columnInt64 = undefined

columnDouble :: Statement -> Int -> IO Double
columnDouble = undefined

columnText :: Statement -> Int -> IO Utf8
columnText = undefined

columnBlob :: Statement -> Int -> IO ByteString
columnBlob = undefined
