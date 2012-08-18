{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.SQLite3.Bindings.Types (
    -- * Objects
    -- | <http://www.sqlite.org/c3ref/objlist.html>
    CDatabase,
    CStatement,

    -- * Enumerations

    -- ** Error
    CError(..),
    decodeError,
    Error(..),

    -- ** ColumnType
    CColumnType(..),
    decodeColumnType,
    ColumnType(..),

    -- * Indices
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_TRANSIENT,

    -- * Conversion helpers
    fromParamIndex,
    fromColumnIndex,
    fromColumnCount,
    fromCNumBytes,
    toParamIndex,
    toColumnIndex,
    toColumnCount,
    toCNumBytes,
) where

#include "sqlite3.h"

import Foreign.C.Types
import Foreign.Ptr

-- Result code documentation copied from <http://www.sqlite.org/c3ref/c_abort.html>

data Error = ErrorOK                     -- ^ Successful result
           | ErrorError                  -- ^ SQL error or missing database
           | ErrorInternal               -- ^ Internal logic error in SQLite
           | ErrorPermission             -- ^ Access permission denied
           | ErrorAbort                  -- ^ Callback routine requested an abort
           | ErrorBusy                   -- ^ The database file is locked
           | ErrorLocked                 -- ^ A table in the database is locked
           | ErrorNoMemory               -- ^ A @malloc()@ failed
           | ErrorReadOnly               -- ^ Attempt to write a readonly database
           | ErrorInterrupt              -- ^ Operation terminated by @sqlite3_interrupt()@
           | ErrorIO                     -- ^ Some kind of disk I/O error occurred
           | ErrorCorrupt                -- ^ The database disk image is malformed
           | ErrorNotFound               -- ^ Unknown opcode in @sqlite3_file_control()@
           | ErrorFull                   -- ^ Insertion failed because database is full
           | ErrorCan'tOpen              -- ^ Unable to open the database file
           | ErrorProtocol               -- ^ Database lock protocol error
           | ErrorEmpty                  -- ^ Database is empty
           | ErrorSchema                 -- ^ The database schema changed
           | ErrorTooBig                 -- ^ String or BLOB exceeds size limit
           | ErrorConstraint             -- ^ Abort due to constraint violation
           | ErrorMismatch               -- ^ Data type mismatch
           | ErrorMisuse                 -- ^ Library used incorrectly
           | ErrorNoLargeFileSupport     -- ^ Uses OS features not supported on host
           | ErrorAuthorization          -- ^ Authorization denied
           | ErrorFormat                 -- ^ Auxiliary database format error
           | ErrorRange                  -- ^ 2nd parameter to sqlite3_bind out of range
           | ErrorNotADatabase           -- ^ File opened that is not a database file
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing
           | ErrorUnknown CInt           -- ^ Unknown error code
             deriving (Eq, Show)

data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

-- | <http://www.sqlite.org/c3ref/sqlite3.html>
--
-- @CDatabase@ = @sqlite3@
data CDatabase

-- | <http://www.sqlite.org/c3ref/stmt.html>
--
-- @CStatement@ = @sqlite3_stmt@
data CStatement

-- | Index of a parameter in a parameterized query.  See
-- <http://www.sqlite.org/lang_expr.html#varparam> for the syntax of parameter
-- placeholders, and how parameter indices are assigned.
--
-- Parameter indices start from 1.
newtype ParamIndex = ParamIndex CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | Index of a column in a result set.
--
-- Column indices start from 0.
newtype ColumnIndex = ColumnIndex CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | Number of columns in a result set.
type ColumnCount = ColumnIndex

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

fromParamIndex :: Num a => ParamIndex -> a
fromParamIndex = fromIntegral

fromColumnIndex :: Num a => ColumnIndex -> a
fromColumnIndex = fromIntegral

fromColumnCount :: Num a => ColumnCount -> a
fromColumnCount = fromIntegral

fromCNumBytes :: Num a => CNumBytes -> a
fromCNumBytes = fromIntegral

toParamIndex :: Integral a => a -> ParamIndex
toParamIndex = fromIntegral

toColumnIndex :: Integral a => a -> ColumnIndex
toColumnIndex = fromIntegral

toColumnCount :: Integral a => a -> ColumnCount
toColumnCount = fromIntegral

toCNumBytes :: Integral a => a -> CNumBytes
toCNumBytes = fromIntegral

-- | <http://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)


-- | <http://www.sqlite.org/c3ref/c_abort.html>
newtype CError = CError CInt
    deriving Show

decodeError :: CError -> Error
decodeError (CError n) = case n of
    #{const SQLITE_OK}         -> ErrorOK
    #{const SQLITE_ERROR}      -> ErrorError
    #{const SQLITE_INTERNAL}   -> ErrorInternal
    #{const SQLITE_PERM}       -> ErrorPermission
    #{const SQLITE_ABORT}      -> ErrorAbort
    #{const SQLITE_BUSY}       -> ErrorBusy
    #{const SQLITE_LOCKED}     -> ErrorLocked
    #{const SQLITE_NOMEM}      -> ErrorNoMemory
    #{const SQLITE_READONLY}   -> ErrorReadOnly
    #{const SQLITE_INTERRUPT}  -> ErrorInterrupt
    #{const SQLITE_IOERR}      -> ErrorIO
    #{const SQLITE_CORRUPT}    -> ErrorCorrupt
    #{const SQLITE_NOTFOUND}   -> ErrorNotFound
    #{const SQLITE_FULL}       -> ErrorFull
    #{const SQLITE_CANTOPEN}   -> ErrorCan'tOpen
    #{const SQLITE_PROTOCOL}   -> ErrorProtocol
    #{const SQLITE_EMPTY}      -> ErrorEmpty
    #{const SQLITE_SCHEMA}     -> ErrorSchema
    #{const SQLITE_TOOBIG}     -> ErrorTooBig
    #{const SQLITE_CONSTRAINT} -> ErrorConstraint
    #{const SQLITE_MISMATCH}   -> ErrorMismatch
    #{const SQLITE_MISUSE}     -> ErrorMisuse
    #{const SQLITE_NOLFS}      -> ErrorNoLargeFileSupport
    #{const SQLITE_AUTH}       -> ErrorAuthorization
    #{const SQLITE_FORMAT}     -> ErrorFormat
    #{const SQLITE_RANGE}      -> ErrorRange
    #{const SQLITE_NOTADB}     -> ErrorNotADatabase
    #{const SQLITE_ROW}        -> ErrorRow
    #{const SQLITE_DONE}       -> ErrorDone
    _                          -> ErrorUnknown n


-- | <http://www.sqlite.org/c3ref/c_blob.html>
newtype CColumnType = CColumnType CInt
    deriving Show

decodeColumnType :: CColumnType -> ColumnType
decodeColumnType (CColumnType n) = case n of
    #{const SQLITE_INTEGER} -> IntegerColumn
    #{const SQLITE_FLOAT}   -> FloatColumn
    #{const SQLITE_TEXT}    -> TextColumn
    #{const SQLITE_BLOB}    -> BlobColumn
    #{const SQLITE_NULL}    -> NullColumn
    _                       -> error $ "decodeColumnType " ++ show n
