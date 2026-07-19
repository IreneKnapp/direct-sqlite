{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.SQLite3.Bindings.Types (
    -- * Objects
    -- | <https://www.sqlite.org/c3ref/objlist.html>
    CDatabase,
    CStatement,
    CValue,
    CContext,
    CBlob,
    CBackup,

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

    -- ** ActionCode
    CActionCode(..),
    decodeActionCode,
    encodeActionCode,
    ActionCode(..),

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
    c_SQLITE_STATIC,
    c_SQLITE_TRANSIENT,
    c_SQLITE_UTF8,

    -- * Custom functions
    ArgCount(..),
    ArgIndex,
    CArgCount(..),
    c_SQLITE_DETERMINISTIC,

    -- * Conversion to and from FFI types
    FFIType(..),
) where

#ifdef direct_sqlite_systemlib
#include <sqlite3.h>
#else
#include "cbits/sqlite3.h"
#endif

import GHC.Generics
import Foreign.C.Types
import Foreign.Ptr

-- Result code documentation copied from <https://www.sqlite.org/c3ref/c_abort.html>

-- | <https://www.sqlite.org/c3ref/c_abort.html>
-- <https://www.sqlite.org/c3ref/c_abort_rollback.html>
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
           | ErrorNotice                 -- ^ Notifications from sqlite3_log()
           | ErrorWarning                -- ^ Warnings from sqlite3_log()
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing

           | ErrorErrorMissingCollatingSquence
           | ErrorErrorRetry
           | ErrorErrorSnapshot
           | ErrorIORead
           | ErrorIOShortRead
           | ErrorIOWrite
           | ErrorIOFsync
           | ErrorIODirectoryFsync
           | ErrorIOTruncate
           | ErrorIOFstat
           | ErrorIOUnlock
           | ErrorIOReadLock
           | ErrorIOBlocked
           | ErrorIODelete
           | ErrorIONoMemory
           | ErrorIOAccess
           | ErrorIOCheckReservedLock
           | ErrorIOLock
           | ErrorIOClose
           | ErrorIODirectoryClose
           | ErrorIOShmOpen
           | ErrorIOShmSize
           | ErrorIOShmLock
           | ErrorIOShmMap
           | ErrorIOSeek
           | ErrorIODeleteNoEntity
           | ErrorIOMmap
           | ErrorIOGetTempPath
           | ErrorIOConvertedPath
           | ErrorIOVNode
           | ErrorIOAuth
           | ErrorIOBeginAtomic
           | ErrorIOCommitAtomic
           | ErrorIORollbackAtomic
           | ErrorIOData
           | ErrorIOCorruptFilesystem
           | ErrorLockedSharedCache
           | ErrorLockedVirtualTable
           | ErrorBusyRecovery
           | ErrorBusySnapshot
           | ErrorBusyTimeout
           | ErrorCan'tOpenNotTempDirectory
           | ErrorCan'tOpenIsDirectory
           | ErrorCan'tOpenFullPath
           | ErrorCan'tOpenConvertedPath
           | ErrorCan'tOpenDirtyWriteAheadLog
           | ErrorCan'tOpenSymlink
           | ErrorCorruptVirtualTable
           | ErrorCorruptSequence
           | ErrorCorruptIndex
           | ErrorReadOnlyRecovery
           | ErrorReadOnlyCan'tLock
           | ErrorReadOnlyRollback
           | ErrorReadOnlyDatabaseMoved
           | ErrorReadOnlyCan'tInit
           | ErrorReadOnlyDirectory
           | ErrorAbortRollback
           | ErrorConstraintCheck
           | ErrorConstraintCommitHook
           | ErrorConstraintForeignKey
           | ErrorConstraintFunction
           | ErrorConstraintNotNull
           | ErrorConstraintPrimaryKey
           | ErrorConstraintTrigger
           | ErrorConstraintUnique
           | ErrorConstraintVirtualTable
           | ErrorConstraintRowId
           | ErrorConstraintPinned
           | ErrorConstraintDataType
           | ErrorNoticeRecoverWriteAheadLog
           | ErrorNoticeRecoverRollback
           | ErrorWarningAutoIndex
           | ErrorAuthUser
           | ErrorOkLoadPermanently
             deriving (Eq, Show, Generic)

-- | <https://www.sqlite.org/c3ref/c_blob.html>
data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

-- | <https://www.sqlite.org/c3ref/c_alter_table.html>
data ActionCode = CreateIndexAction
                | CreateTableAction
                | CreateTempIndexAction
                | CreateTempTableAction
                | CreateTempTriggerAction
                | CreateTempViewAction
                | CreateTriggerAction
                | CreateViewAction
                | DeleteAction
                | DropIndexAction
                | DropTableAction
                | DropTempIndexAction
                | DropTempTableAction
                | DropTempTriggerAction
                | DropTempViewAction
                | DropTriggerAction
                | DropViewAction
                | InsertAction
                | PragmaAction
                | ReadAction
                | SelectAction
                | TransactionAction
                | UpdateAction
                | AttachAction
                | DetachAction
                | AlterTableAction
                | ReindexAction
                | AnalyzeAction
                | CreateVtableAction
                | DropVtableAction
                | FunctionAction
                | SavepointAction
                | CopyAction
                | RecursiveAction

-- | <https://www.sqlite.org/c3ref/sqlite3.html>
--
-- @CDatabase@ = @sqlite3@
data CDatabase

-- | <https://www.sqlite.org/c3ref/stmt.html>
--
-- @CStatement@ = @sqlite3_stmt@
data CStatement

-- | <https://www.sqlite.org/c3ref/value.html>
--
-- @CValue@ = @sqlite3_value@
data CValue

-- | <https://www.sqlite.org/c3ref/context.html>
--
-- @CContext@ = @sqlite3_context@
data CContext

-- | <https://www.sqlite.org/c3ref/blob.html>
--
-- @CBlob@ = @sqlite3_blob@
data CBlob

-- | <https://www.sqlite.org/c3ref/backup.html>
--
-- @CBackup@ = @sqlite3_backup@
data CBackup

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
-- See <https://www.sqlite.org/lang_expr.html#varparam> for the syntax of
-- parameter placeholders, and how parameter indices are assigned.
newtype ParamIndex = ParamIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ParamIndex where
    show (ParamIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ParamIndex where
    minBound = ParamIndex (fromIntegral (minBound :: CInt))
    maxBound = ParamIndex (fromIntegral (maxBound :: CInt))

-- | Index of a column in a result set.  Column indices start from 0.
newtype ColumnIndex = ColumnIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ColumnIndex where
    show (ColumnIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ColumnIndex where
    minBound = ColumnIndex (fromIntegral (minBound :: CInt))
    maxBound = ColumnIndex (fromIntegral (maxBound :: CInt))

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

-- | <https://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

-- | Tells SQLite3 that the content pointer is constant and will never change
c_SQLITE_STATIC :: Ptr CDestructor
c_SQLITE_STATIC = intPtrToPtr 0

-- | Tells SQLite3 to make its own private copy of the data
c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)

c_SQLITE_UTF8 :: CInt
c_SQLITE_UTF8 = #{const SQLITE_UTF8}

-- | Number of arguments of a user defined SQL function.
newtype ArgCount = ArgCount Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ArgCount where
    show (ArgCount n) = show n

instance Bounded ArgCount where
    minBound = ArgCount 0
    maxBound = ArgCount (#{const SQLITE_LIMIT_FUNCTION_ARG})

-- | Index of an argument to a custom function. Indices start from 0.
type ArgIndex = ArgCount

newtype CArgCount = CArgCount CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CArgCount where
    show (CArgCount n) = show n

instance Bounded CArgCount where
    minBound = CArgCount (-1)
    maxBound = CArgCount #{const SQLITE_LIMIT_FUNCTION_ARG}

-- | Tells SQLite3 that the defined custom SQL function is deterministic.
c_SQLITE_DETERMINISTIC :: CInt
c_SQLITE_DETERMINISTIC = #{const SQLITE_DETERMINISTIC}

-- | <https://www.sqlite.org/c3ref/c_abort.html>
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
-- the one bundled (currently, 3.24.0).  If you do, ensure that 'decodeError'
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
    #{const SQLITE_NOTICE}     -> ErrorNotice
    #{const SQLITE_WARNING}    -> ErrorWarning
    #{const SQLITE_ROW}        -> ErrorRow
    #{const SQLITE_DONE}       -> ErrorDone
    -- extended result codes
    #{const SQLITE_ERROR_MISSING_COLLSEQ}   -> ErrorErrorMissingCollatingSquence
    #{const SQLITE_ERROR_RETRY}             -> ErrorErrorRetry
    #{const SQLITE_ERROR_SNAPSHOT}          -> ErrorErrorSnapshot
    #{const SQLITE_IOERR_READ}              -> ErrorIORead
    #{const SQLITE_IOERR_SHORT_READ}        -> ErrorIOShortRead
    #{const SQLITE_IOERR_WRITE}             -> ErrorIOWrite
    #{const SQLITE_IOERR_FSYNC}             -> ErrorIOFsync
    #{const SQLITE_IOERR_DIR_FSYNC}         -> ErrorIODirectoryFsync
    #{const SQLITE_IOERR_TRUNCATE}          -> ErrorIOTruncate
    #{const SQLITE_IOERR_FSTAT}             -> ErrorIOFstat
    #{const SQLITE_IOERR_UNLOCK}            -> ErrorIOUnlock
    #{const SQLITE_IOERR_RDLOCK}            -> ErrorIOReadLock
    #{const SQLITE_IOERR_DELETE}            -> ErrorIODelete
    #{const SQLITE_IOERR_BLOCKED}           -> ErrorIOBlocked
    #{const SQLITE_IOERR_NOMEM}             -> ErrorIONoMemory
    #{const SQLITE_IOERR_ACCESS}            -> ErrorIOAccess
    #{const SQLITE_IOERR_CHECKRESERVEDLOCK} -> ErrorIOCheckReservedLock
    #{const SQLITE_IOERR_LOCK}              -> ErrorIOLock
    #{const SQLITE_IOERR_CLOSE}             -> ErrorIOClose
    #{const SQLITE_IOERR_DIR_CLOSE}         -> ErrorIODirectoryClose
    #{const SQLITE_IOERR_SHMOPEN}           -> ErrorIOShmOpen
    #{const SQLITE_IOERR_SHMSIZE}           -> ErrorIOShmSize
    #{const SQLITE_IOERR_SHMLOCK}           -> ErrorIOShmLock
    #{const SQLITE_IOERR_SHMMAP}            -> ErrorIOShmMap
    #{const SQLITE_IOERR_SEEK}              -> ErrorIOSeek
    #{const SQLITE_IOERR_DELETE_NOENT}      -> ErrorIODeleteNoEntity
    #{const SQLITE_IOERR_MMAP}              -> ErrorIOMmap
    #{const SQLITE_IOERR_GETTEMPPATH}       -> ErrorIOGetTempPath
    #{const SQLITE_IOERR_CONVPATH}          -> ErrorIOConvertedPath
    #{const SQLITE_IOERR_VNODE}             -> ErrorIOVNode
    #{const SQLITE_IOERR_AUTH}              -> ErrorIOAuth
    #{const SQLITE_IOERR_BEGIN_ATOMIC}      -> ErrorIOBeginAtomic
    #{const SQLITE_IOERR_COMMIT_ATOMIC}     -> ErrorIOCommitAtomic
    #{const SQLITE_IOERR_ROLLBACK_ATOMIC}   -> ErrorIORollbackAtomic
    #{const SQLITE_IOERR_DATA}              -> ErrorIOData
    #{const SQLITE_IOERR_CORRUPTFS}         -> ErrorIOCorruptFilesystem
    #{const SQLITE_LOCKED_SHAREDCACHE}      -> ErrorLockedSharedCache
    #{const SQLITE_LOCKED_VTAB}             -> ErrorLockedVirtualTable
    #{const SQLITE_BUSY_RECOVERY}           -> ErrorBusyRecovery
    #{const SQLITE_BUSY_SNAPSHOT}           -> ErrorBusySnapshot
    #{const SQLITE_BUSY_TIMEOUT}            -> ErrorBusyTimeout
    #{const SQLITE_CANTOPEN_NOTEMPDIR}      -> ErrorCan'tOpenNotTempDirectory
    #{const SQLITE_CANTOPEN_ISDIR}          -> ErrorCan'tOpenIsDirectory
    #{const SQLITE_CANTOPEN_FULLPATH}       -> ErrorCan'tOpenFullPath
    #{const SQLITE_CANTOPEN_CONVPATH}       -> ErrorCan'tOpenConvertedPath
    #{const SQLITE_CANTOPEN_DIRTYWAL}       -> ErrorCan'tOpenDirtyWriteAheadLog
    #{const SQLITE_CANTOPEN_SYMLINK}        -> ErrorCan'tOpenSymlink
    #{const SQLITE_CORRUPT_VTAB}            -> ErrorCorruptVirtualTable
    #{const SQLITE_CORRUPT_SEQUENCE}        -> ErrorCorruptSequence
    #{const SQLITE_CORRUPT_INDEX}           -> ErrorCorruptIndex
    #{const SQLITE_READONLY_RECOVERY}       -> ErrorReadOnlyRecovery
    #{const SQLITE_READONLY_CANTLOCK}       -> ErrorReadOnlyCan'tLock
    #{const SQLITE_READONLY_ROLLBACK}       -> ErrorReadOnlyRollback
    #{const SQLITE_READONLY_DBMOVED}        -> ErrorReadOnlyDatabaseMoved
    #{const SQLITE_READONLY_CANTINIT}       -> ErrorReadOnlyCan'tInit
    #{const SQLITE_READONLY_DIRECTORY}      -> ErrorReadOnlyDirectory
    #{const SQLITE_ABORT_ROLLBACK}          -> ErrorAbortRollback
    #{const SQLITE_CONSTRAINT_CHECK}        -> ErrorConstraintCheck
    #{const SQLITE_CONSTRAINT_COMMITHOOK}   -> ErrorConstraintCommitHook
    #{const SQLITE_CONSTRAINT_FOREIGNKEY}   -> ErrorConstraintForeignKey
    #{const SQLITE_CONSTRAINT_FUNCTION}     -> ErrorConstraintFunction
    #{const SQLITE_CONSTRAINT_NOTNULL}      -> ErrorConstraintNotNull
    #{const SQLITE_CONSTRAINT_PRIMARYKEY}   -> ErrorConstraintPrimaryKey
    #{const SQLITE_CONSTRAINT_TRIGGER}      -> ErrorConstraintTrigger
    #{const SQLITE_CONSTRAINT_UNIQUE}       -> ErrorConstraintUnique
    #{const SQLITE_CONSTRAINT_VTAB}         -> ErrorConstraintVirtualTable
    #{const SQLITE_CONSTRAINT_ROWID}        -> ErrorConstraintRowId
    #{const SQLITE_CONSTRAINT_PINNED}       -> ErrorConstraintPinned
    #{const SQLITE_CONSTRAINT_DATATYPE}     -> ErrorConstraintDataType
    #{const SQLITE_NOTICE_RECOVER_WAL}      -> ErrorNoticeRecoverWriteAheadLog
    #{const SQLITE_NOTICE_RECOVER_ROLLBACK} -> ErrorNoticeRecoverRollback
    #{const SQLITE_WARNING_AUTOINDEX}       -> ErrorWarningAutoIndex
    #{const SQLITE_AUTH_USER}               -> ErrorAuthUser
    #{const SQLITE_OK_LOAD_PERMANENTLY}     -> ErrorOkLoadPermanently
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
    ErrorNotice             -> #const SQLITE_NOTICE
    ErrorWarning            -> #const SQLITE_WARNING
    ErrorRow                -> #const SQLITE_ROW
    ErrorDone               -> #const SQLITE_DONE

    ErrorErrorMissingCollatingSquence  -> #const SQLITE_ERROR_MISSING_COLLSEQ
    ErrorErrorRetry                    -> #const SQLITE_ERROR_RETRY
    ErrorErrorSnapshot                 -> #const SQLITE_ERROR_SNAPSHOT
    ErrorIORead                        -> #const SQLITE_IOERR_READ
    ErrorIOShortRead                   -> #const SQLITE_IOERR_SHORT_READ
    ErrorIOWrite                       -> #const SQLITE_IOERR_WRITE
    ErrorIOFsync                       -> #const SQLITE_IOERR_FSYNC
    ErrorIODirectoryFsync              -> #const SQLITE_IOERR_DIR_FSYNC
    ErrorIOTruncate                    -> #const SQLITE_IOERR_TRUNCATE
    ErrorIOFstat                       -> #const SQLITE_IOERR_FSTAT
    ErrorIOUnlock                      -> #const SQLITE_IOERR_UNLOCK
    ErrorIOReadLock                    -> #const SQLITE_IOERR_RDLOCK
    ErrorIOBlocked                     -> #const SQLITE_IOERR_BLOCKED
    ErrorIODelete                      -> #const SQLITE_IOERR_DELETE
    ErrorIONoMemory                    -> #const SQLITE_IOERR_NOMEM
    ErrorIOAccess                      -> #const SQLITE_IOERR_ACCESS
    ErrorIOCheckReservedLock           -> #const SQLITE_IOERR_CHECKRESERVEDLOCK
    ErrorIOLock                        -> #const SQLITE_IOERR_LOCK
    ErrorIOClose                       -> #const SQLITE_IOERR_CLOSE
    ErrorIODirectoryClose              -> #const SQLITE_IOERR_DIR_CLOSE
    ErrorIOShmOpen                     -> #const SQLITE_IOERR_SHMOPEN
    ErrorIOShmSize                     -> #const SQLITE_IOERR_SHMSIZE
    ErrorIOShmLock                     -> #const SQLITE_IOERR_SHMLOCK
    ErrorIOShmMap                      -> #const SQLITE_IOERR_SHMMAP
    ErrorIOSeek                        -> #const SQLITE_IOERR_SEEK
    ErrorIODeleteNoEntity              -> #const SQLITE_IOERR_DELETE_NOENT
    ErrorIOMmap                        -> #const SQLITE_IOERR_MMAP
    ErrorIOGetTempPath                 -> #const SQLITE_IOERR_GETTEMPPATH
    ErrorIOConvertedPath               -> #const SQLITE_IOERR_CONVPATH
    ErrorIOVNode                       -> #const SQLITE_IOERR_VNODE
    ErrorIOAuth                        -> #const SQLITE_IOERR_AUTH
    ErrorIOBeginAtomic                 -> #const SQLITE_IOERR_BEGIN_ATOMIC
    ErrorIOCommitAtomic                -> #const SQLITE_IOERR_COMMIT_ATOMIC
    ErrorIORollbackAtomic              -> #const SQLITE_IOERR_ROLLBACK_ATOMIC
    ErrorIOData                        -> #const SQLITE_IOERR_DATA
    ErrorIOCorruptFilesystem           -> #const SQLITE_IOERR_CORRUPTFS
    ErrorLockedSharedCache             -> #const SQLITE_LOCKED_SHAREDCACHE
    ErrorLockedVirtualTable            -> #const SQLITE_LOCKED_VTAB
    ErrorBusyRecovery                  -> #const SQLITE_BUSY_RECOVERY
    ErrorBusySnapshot                  -> #const SQLITE_BUSY_SNAPSHOT
    ErrorBusyTimeout                   -> #const SQLITE_BUSY_TIMEOUT
    ErrorCan'tOpenNotTempDirectory     -> #const SQLITE_CANTOPEN_NOTEMPDIR
    ErrorCan'tOpenIsDirectory          -> #const SQLITE_CANTOPEN_ISDIR
    ErrorCan'tOpenFullPath             -> #const SQLITE_CANTOPEN_FULLPATH
    ErrorCan'tOpenConvertedPath        -> #const SQLITE_CANTOPEN_CONVPATH
    ErrorCan'tOpenDirtyWriteAheadLog   -> #const SQLITE_CANTOPEN_DIRTYWAL
    ErrorCan'tOpenSymlink              -> #const SQLITE_CANTOPEN_SYMLINK
    ErrorCorruptVirtualTable           -> #const SQLITE_CORRUPT_VTAB
    ErrorCorruptSequence               -> #const SQLITE_CORRUPT_SEQUENCE
    ErrorCorruptIndex                  -> #const SQLITE_CORRUPT_INDEX
    ErrorReadOnlyRecovery              -> #const SQLITE_READONLY_RECOVERY
    ErrorReadOnlyCan'tLock             -> #const SQLITE_READONLY_CANTLOCK
    ErrorReadOnlyRollback              -> #const SQLITE_READONLY_ROLLBACK
    ErrorReadOnlyDatabaseMoved         -> #const SQLITE_READONLY_DBMOVED
    ErrorReadOnlyCan'tInit             -> #const SQLITE_READONLY_CANTINIT
    ErrorReadOnlyDirectory             -> #const SQLITE_READONLY_DIRECTORY
    ErrorAbortRollback                 -> #const SQLITE_ABORT_ROLLBACK
    ErrorConstraintCheck               -> #const SQLITE_CONSTRAINT_CHECK
    ErrorConstraintCommitHook          -> #const SQLITE_CONSTRAINT_COMMITHOOK
    ErrorConstraintForeignKey          -> #const SQLITE_CONSTRAINT_FOREIGNKEY
    ErrorConstraintFunction            -> #const SQLITE_CONSTRAINT_FUNCTION
    ErrorConstraintNotNull             -> #const SQLITE_CONSTRAINT_NOTNULL
    ErrorConstraintPrimaryKey          -> #const SQLITE_CONSTRAINT_PRIMARYKEY
    ErrorConstraintTrigger             -> #const SQLITE_CONSTRAINT_TRIGGER
    ErrorConstraintUnique              -> #const SQLITE_CONSTRAINT_UNIQUE
    ErrorConstraintVirtualTable        -> #const SQLITE_CONSTRAINT_VTAB
    ErrorConstraintRowId               -> #const SQLITE_CONSTRAINT_ROWID
    ErrorConstraintPinned              -> #const SQLITE_CONSTRAINT_PINNED
    ErrorConstraintDataType            -> #const SQLITE_CONSTRAINT_DATATYPE
    ErrorNoticeRecoverWriteAheadLog    -> #const SQLITE_NOTICE_RECOVER_WAL
    ErrorNoticeRecoverRollback         -> #const SQLITE_NOTICE_RECOVER_ROLLBACK
    ErrorWarningAutoIndex              -> #const SQLITE_WARNING_AUTOINDEX
    ErrorAuthUser                      -> #const SQLITE_AUTH_USER
    ErrorOkLoadPermanently             -> #const SQLITE_OK_LOAD_PERMANENTLY

-- | <https://www.sqlite.org/c3ref/c_blob.html>
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

-- | <https://www.sqlite.org/c3ref/c_alter_table.html>
newtype CActionCode =  CActionCode CInt
    deriving (Eq, Show)

decodeActionCode :: CActionCode -> ActionCode
decodeActionCode (CActionCode n) = case n of
    #{const SQLITE_CREATE_INDEX}        -> CreateIndexAction
    #{const SQLITE_CREATE_TABLE}        -> CreateTableAction
    #{const SQLITE_CREATE_TEMP_INDEX}   -> CreateTempIndexAction
    #{const SQLITE_CREATE_TEMP_TABLE}   -> CreateTempTableAction
    #{const SQLITE_CREATE_TEMP_TRIGGER} -> CreateTempTriggerAction
    #{const SQLITE_CREATE_TEMP_VIEW}    -> CreateTempViewAction
    #{const SQLITE_CREATE_TRIGGER}      -> CreateTriggerAction
    #{const SQLITE_CREATE_VIEW}         -> CreateViewAction
    #{const SQLITE_DELETE}              -> DeleteAction
    #{const SQLITE_DROP_INDEX}          -> DropIndexAction
    #{const SQLITE_DROP_TABLE}          -> DropTableAction
    #{const SQLITE_DROP_TEMP_INDEX}     -> DropTempIndexAction
    #{const SQLITE_DROP_TEMP_TABLE}     -> DropTempTableAction
    #{const SQLITE_DROP_TEMP_TRIGGER}   -> DropTempTriggerAction
    #{const SQLITE_DROP_TEMP_VIEW}      -> DropTempViewAction
    #{const SQLITE_DROP_TRIGGER}        -> DropTriggerAction
    #{const SQLITE_DROP_VIEW}           -> DropViewAction
    #{const SQLITE_INSERT}              -> InsertAction
    #{const SQLITE_PRAGMA}              -> PragmaAction
    #{const SQLITE_READ}                -> ReadAction
    #{const SQLITE_SELECT}              -> SelectAction
    #{const SQLITE_TRANSACTION}         -> TransactionAction
    #{const SQLITE_UPDATE}              -> UpdateAction
    #{const SQLITE_ATTACH}              -> AttachAction
    #{const SQLITE_DETACH}              -> DetachAction
    #{const SQLITE_ALTER_TABLE}         -> AlterTableAction
    #{const SQLITE_REINDEX}             -> ReindexAction
    #{const SQLITE_ANALYZE}             -> AnalyzeAction
    #{const SQLITE_CREATE_VTABLE}       -> CreateVtableAction
    #{const SQLITE_DROP_VTABLE}         -> DropVtableAction
    #{const SQLITE_FUNCTION}            -> FunctionAction
    #{const SQLITE_SAVEPOINT}           -> SavepointAction
    #{const SQLITE_COPY}                -> CopyAction
    #{const SQLITE_RECURSIVE}           -> RecursiveAction
    _                                   -> error $ "decodeActionCode " ++ show n

encodeActionCode :: ActionCode -> CActionCode
encodeActionCode t = CActionCode $ case t of
    CreateIndexAction       -> #const SQLITE_CREATE_INDEX
    CreateTableAction       -> #const SQLITE_CREATE_TABLE
    CreateTempIndexAction   -> #const SQLITE_CREATE_TEMP_INDEX
    CreateTempTableAction   -> #const SQLITE_CREATE_TEMP_TABLE
    CreateTempTriggerAction -> #const SQLITE_CREATE_TEMP_TRIGGER
    CreateTempViewAction    -> #const SQLITE_CREATE_TEMP_VIEW
    CreateTriggerAction     -> #const SQLITE_CREATE_TRIGGER
    CreateViewAction        -> #const SQLITE_CREATE_VIEW
    DeleteAction            -> #const SQLITE_DELETE
    DropIndexAction         -> #const SQLITE_DROP_INDEX
    DropTableAction         -> #const SQLITE_DROP_TABLE
    DropTempIndexAction     -> #const SQLITE_DROP_TEMP_INDEX
    DropTempTableAction     -> #const SQLITE_DROP_TEMP_TABLE
    DropTempTriggerAction   -> #const SQLITE_DROP_TEMP_TRIGGER
    DropTempViewAction      -> #const SQLITE_DROP_TEMP_VIEW
    DropTriggerAction       -> #const SQLITE_DROP_TRIGGER
    DropViewAction          -> #const SQLITE_DROP_VIEW
    InsertAction            -> #const SQLITE_INSERT
    PragmaAction            -> #const SQLITE_PRAGMA
    ReadAction              -> #const SQLITE_READ
    SelectAction            -> #const SQLITE_SELECT
    TransactionAction       -> #const SQLITE_TRANSACTION
    UpdateAction            -> #const SQLITE_UPDATE
    AttachAction            -> #const SQLITE_ATTACH
    DetachAction            -> #const SQLITE_DETACH
    AlterTableAction        -> #const SQLITE_ALTER_TABLE
    ReindexAction           -> #const SQLITE_REINDEX
    AnalyzeAction           -> #const SQLITE_ANALYZE
    CreateVtableAction      -> #const SQLITE_CREATE_VTABLE
    DropVtableAction        -> #const SQLITE_DROP_VTABLE
    FunctionAction          -> #const SQLITE_FUNCTION
    SavepointAction         -> #const SQLITE_SAVEPOINT
    CopyAction              -> #const SQLITE_COPY
    RecursiveAction         -> #const SQLITE_RECURSIVE

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

instance FFIType ActionCode CActionCode where
    toFFI = encodeActionCode
    fromFFI = decodeActionCode

instance FFIType ArgCount CArgCount where
    toFFI (ArgCount n)  = CArgCount (fromIntegral n)
    fromFFI (CArgCount n) = ArgCount (fromIntegral n)
