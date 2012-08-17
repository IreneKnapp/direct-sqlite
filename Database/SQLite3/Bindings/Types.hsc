{-# LANGUAGE EmptyDataDecls #-}
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
    CParamIndex(..),
    CColumnIndex(..),
    CColumnCount(..),

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_TRANSIENT,

    -- * Conversion helpers
    fromCParamIndex,
    fromCColumnIndex,
    fromCColumnCount,
    fromCNumBytes,
    toCParamIndex,
    toCColumnIndex,
    toCColumnCount,
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
newtype CParamIndex = CParamIndex CInt
    deriving (Eq, Ord, Show)

-- | Index of a column in a result set.
--
-- Column indices start from 0.
newtype CColumnIndex = CColumnIndex CInt
    deriving (Eq, Ord, Show)

-- | Number of columns in a result set.
newtype CColumnCount = CColumnCount CInt
    deriving (Eq, Ord, Show)

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show)

fromCParamIndex :: Num a => CParamIndex -> a
fromCParamIndex (CParamIndex n) = fromIntegral n

fromCColumnIndex :: Num a => CColumnIndex -> a
fromCColumnIndex (CColumnIndex n) = fromIntegral n

fromCColumnCount :: Num a => CColumnCount -> a
fromCColumnCount (CColumnCount n) = fromIntegral n

fromCNumBytes :: Num a => CNumBytes -> a
fromCNumBytes (CNumBytes n) = fromIntegral n

toCParamIndex :: Integral a => a -> CParamIndex
toCParamIndex n = CParamIndex (fromIntegral n)

toCColumnIndex :: Integral a => a -> CColumnIndex
toCColumnIndex n = CColumnIndex (fromIntegral n)

toCColumnCount :: Integral a => a -> CColumnCount
toCColumnCount n = CColumnCount (fromIntegral n)

toCNumBytes :: Integral a => a -> CNumBytes
toCNumBytes n = CNumBytes (fromIntegral n)

-- | <http://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)


-- | <http://www.sqlite.org/c3ref/c_abort.html>
newtype CError = CError CInt
    deriving Show

instance Enum CError where
    toEnum n            = CError (toEnum n)
    fromEnum (CError n) = fromEnum n

decodeError :: CError -> Maybe Error
decodeError (CError n) = case n of
    #{const SQLITE_OK}         -> Just ErrorOK
    #{const SQLITE_ERROR}      -> Just ErrorError
    #{const SQLITE_INTERNAL}   -> Just ErrorInternal
    #{const SQLITE_PERM}       -> Just ErrorPermission
    #{const SQLITE_ABORT}      -> Just ErrorAbort
    #{const SQLITE_BUSY}       -> Just ErrorBusy
    #{const SQLITE_LOCKED}     -> Just ErrorLocked
    #{const SQLITE_NOMEM}      -> Just ErrorNoMemory
    #{const SQLITE_READONLY}   -> Just ErrorReadOnly
    #{const SQLITE_INTERRUPT}  -> Just ErrorInterrupt
    #{const SQLITE_IOERR}      -> Just ErrorIO
    #{const SQLITE_CORRUPT}    -> Just ErrorCorrupt
    #{const SQLITE_NOTFOUND}   -> Just ErrorNotFound
    #{const SQLITE_FULL}       -> Just ErrorFull
    #{const SQLITE_CANTOPEN}   -> Just ErrorCan'tOpen
    #{const SQLITE_PROTOCOL}   -> Just ErrorProtocol
    #{const SQLITE_EMPTY}      -> Just ErrorEmpty
    #{const SQLITE_SCHEMA}     -> Just ErrorSchema
    #{const SQLITE_TOOBIG}     -> Just ErrorTooBig
    #{const SQLITE_CONSTRAINT} -> Just ErrorConstraint
    #{const SQLITE_MISMATCH}   -> Just ErrorMismatch
    #{const SQLITE_MISUSE}     -> Just ErrorMisuse
    #{const SQLITE_NOLFS}      -> Just ErrorNoLargeFileSupport
    #{const SQLITE_AUTH}       -> Just ErrorAuthorization
    #{const SQLITE_FORMAT}     -> Just ErrorFormat
    #{const SQLITE_RANGE}      -> Just ErrorRange
    #{const SQLITE_NOTADB}     -> Just ErrorNotADatabase
    #{const SQLITE_ROW}        -> Just ErrorRow
    #{const SQLITE_DONE}       -> Just ErrorDone
    _                          -> Nothing


-- | <http://www.sqlite.org/c3ref/c_blob.html>
newtype CColumnType = CColumnType CInt
    deriving Show

decodeColumnType :: CColumnType -> Maybe ColumnType
decodeColumnType (CColumnType n) = case n of
    #{const SQLITE_INTEGER} -> Just IntegerColumn
    #{const SQLITE_FLOAT}   -> Just FloatColumn
    #{const SQLITE_TEXT}    -> Just TextColumn
    #{const SQLITE_BLOB}    -> Just BlobColumn
    #{const SQLITE_NULL}    -> Just NullColumn
    _                       -> Nothing
