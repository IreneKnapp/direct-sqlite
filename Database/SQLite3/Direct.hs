{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Database.SQLite3.Direct (
    -- * Types
    Database,
    Statement,
    SQLData(..),

    -- ** Results and errors
    Result(..),
    StepResult(..),
    Error(..),

{-
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
-}
) where

import Database.SQLite3.Bindings

import Prelude hiding (error)
import qualified Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative ((<$>))
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
    | SQLText       !T.Text
    | SQLBlob       !BS.ByteString
    | SQLNull
    deriving (Eq, Show, Typeable)

-- | Convert a 'CError' to a 'Result', in the common case where @SQLITE_OK@
-- signals success and anything else signals an error.
toResult :: CError -> a -> Result a
toResult code@(CError n) a =
    case decodeError code of
        Just ErrorOK -> Okay a
        Just err     -> Error err
        Nothing      -> UnknownError (fromIntegral n)

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
