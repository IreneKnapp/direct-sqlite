{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    encodeError,
    Error(..),

    -- ** ColumnType
    CColumnType(..),
    decodeColumnType,
    encodeColumnType,
    ColumnType(..),

    -- * Indices
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,

    -- ** Indices (FFI)
    CParamIndex(..),
    CColumnIndex(..),
    CColumnCount,

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_TRANSIENT,

    -- * Conversion to and from FFI types
    FFIType(..),
) where

#ifdef direct_sqlite_systemlib
#include <sqlite3.h>
#else
#include "cbits/sqlite3.h"
#endif

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

-- | Index of a parameter in a parameterized query.
-- Parameter indices start from 1.
--
-- When a query is 'Database.SQLite3.prepare'd, SQLite allocates an
-- array indexed from 1 to the highest parameter index.  For example:
--
-- >>Right stmt <- prepare conn "SELECT ?1, ?5, ?3, ?"
-- >>bindParameterCount stmt
-- >ParamIndex 6
--
-- This will allocate an array indexed from 1 to 6 (@?@ takes the highest
-- preceding index plus one).  The array is initialized with null values.
-- When you bind a parameter with 'Database.SQLite3.bindSQLData', it assigns a
-- new value to one of these indices.
--
-- See <http://www.sqlite.org/lang_expr.html#varparam> for the syntax of
-- parameter placeholders, and how parameter indices are assigned.
newtype ParamIndex = ParamIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ParamIndex where
    show (ParamIndex n) = show n

-- | Index of a column in a result set.  Column indices start from 0.
newtype ColumnIndex = ColumnIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ColumnIndex where
    show (ColumnIndex n) = show n

-- | Number of columns in a result set.
type ColumnCount = ColumnIndex

newtype CParamIndex = CParamIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CParamIndex where
    show (CParamIndex n) = show n

newtype CColumnIndex = CColumnIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CColumnIndex where
    show (CColumnIndex n) = show n

type CColumnCount = CColumnIndex

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | <http://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

-- | Tells SQLite3 to make its own private copy of the data
c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)


-- | <http://www.sqlite.org/c3ref/c_abort.html>
newtype CError = CError CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.  If the error code is invalid, or
-- perhaps introduced in a newer version of SQLite but this library has not
-- been updated to support it, the result is undefined.
--
-- To be clear, if 'decodeError' fails, it is /undefined behavior/, not an
-- exception you can handle.
--
-- Therefore, do not use direct-sqlite with a different version of SQLite than
-- the one bundled (currently, 3.7.13).  If you do, ensure that 'decodeError'
-- and 'decodeColumnType' are still exhaustive.
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
    _                          -> error $ "decodeError " ++ show n

encodeError :: Error -> CError
encodeError err = CError $ case err of
    ErrorOK                 -> #const SQLITE_OK
    ErrorError              -> #const SQLITE_ERROR
    ErrorInternal           -> #const SQLITE_INTERNAL
    ErrorPermission         -> #const SQLITE_PERM
    ErrorAbort              -> #const SQLITE_ABORT
    ErrorBusy               -> #const SQLITE_BUSY
    ErrorLocked             -> #const SQLITE_LOCKED
    ErrorNoMemory           -> #const SQLITE_NOMEM
    ErrorReadOnly           -> #const SQLITE_READONLY
    ErrorInterrupt          -> #const SQLITE_INTERRUPT
    ErrorIO                 -> #const SQLITE_IOERR
    ErrorCorrupt            -> #const SQLITE_CORRUPT
    ErrorNotFound           -> #const SQLITE_NOTFOUND
    ErrorFull               -> #const SQLITE_FULL
    ErrorCan'tOpen          -> #const SQLITE_CANTOPEN
    ErrorProtocol           -> #const SQLITE_PROTOCOL
    ErrorEmpty              -> #const SQLITE_EMPTY
    ErrorSchema             -> #const SQLITE_SCHEMA
    ErrorTooBig             -> #const SQLITE_TOOBIG
    ErrorConstraint         -> #const SQLITE_CONSTRAINT
    ErrorMismatch           -> #const SQLITE_MISMATCH
    ErrorMisuse             -> #const SQLITE_MISUSE
    ErrorNoLargeFileSupport -> #const SQLITE_NOLFS
    ErrorAuthorization      -> #const SQLITE_AUTH
    ErrorFormat             -> #const SQLITE_FORMAT
    ErrorRange              -> #const SQLITE_RANGE
    ErrorNotADatabase       -> #const SQLITE_NOTADB
    ErrorRow                -> #const SQLITE_ROW
    ErrorDone               -> #const SQLITE_DONE


-- | <http://www.sqlite.org/c3ref/c_blob.html>
newtype CColumnType = CColumnType CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.
-- See 'decodeError' for more information.
decodeColumnType :: CColumnType -> ColumnType
decodeColumnType (CColumnType n) = case n of
    #{const SQLITE_INTEGER} -> IntegerColumn
    #{const SQLITE_FLOAT}   -> FloatColumn
    #{const SQLITE_TEXT}    -> TextColumn
    #{const SQLITE_BLOB}    -> BlobColumn
    #{const SQLITE_NULL}    -> NullColumn
    _                       -> error $ "decodeColumnType " ++ show n

encodeColumnType :: ColumnType -> CColumnType
encodeColumnType t = CColumnType $ case t of
    IntegerColumn -> #const SQLITE_INTEGER
    FloatColumn   -> #const SQLITE_FLOAT
    TextColumn    -> #const SQLITE_TEXT
    BlobColumn    -> #const SQLITE_BLOB
    NullColumn    -> #const SQLITE_NULL

------------------------------------------------------------------------
-- Conversion to and from FFI types

-- | The "Database.SQLite3" and "Database.SQLite3.Direct" modules use
-- higher-level representations of some types than those used in the
-- FFI signatures ("Database.SQLite3.Bindings").  This typeclass
-- helps with the conversions.
class FFIType public ffi | public -> ffi, ffi -> public where
    toFFI   :: public -> ffi
    fromFFI :: ffi -> public

instance FFIType ParamIndex CParamIndex where
    toFFI (ParamIndex n) = CParamIndex (fromIntegral n)
    fromFFI (CParamIndex n) = ParamIndex (fromIntegral n)

instance FFIType ColumnIndex CColumnIndex where
    toFFI (ColumnIndex n) = CColumnIndex (fromIntegral n)
    fromFFI (CColumnIndex n) = ColumnIndex (fromIntegral n)

instance FFIType Error CError where
    toFFI = encodeError
    fromFFI = decodeError

instance FFIType ColumnType CColumnType where
    toFFI = encodeColumnType
    fromFFI = decodeColumnType
