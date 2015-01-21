{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite3.Bindings (
    module Database.SQLite3.Bindings.Types,

    -- * Connection management
    c_sqlite3_open,
    c_sqlite3_close,
    c_sqlite3_errmsg,
    c_sqlite3_interrupt,
    c_sqlite3_trace,
    CTraceCallback,
    mkCTraceCallback,
    c_sqlite3_get_autocommit,
    c_sqlite3_enable_shared_cache,

    -- * Simple query execution
    -- | <http://sqlite.org/c3ref/exec.html>
    c_sqlite3_exec,
    CExecCallback,
    mkCExecCallback,

    -- * Statement management
    c_sqlite3_prepare_v2,
    c_sqlite3_db_handle,
    c_sqlite3_step,
    c_sqlite3_reset,
    c_sqlite3_finalize,
    c_sqlite3_clear_bindings,
    c_sqlite3_sql,

    -- * Parameter and column information
    c_sqlite3_bind_parameter_count,
    c_sqlite3_bind_parameter_name,
    c_sqlite3_bind_parameter_index,
    c_sqlite3_column_count,
    c_sqlite3_column_name,

    -- * Binding Values To Prepared Statements
    -- | <http://www.sqlite.org/c3ref/bind_blob.html>
    c_sqlite3_bind_blob,
    c_sqlite3_bind_zeroblob,
    c_sqlite3_bind_text,
    c_sqlite3_bind_double,
    c_sqlite3_bind_int64,
    c_sqlite3_bind_null,

    -- * Result Values From A Query
    -- | <http://www.sqlite.org/c3ref/column_blob.html>
    c_sqlite3_column_type,
    c_sqlite3_column_bytes,
    c_sqlite3_column_blob,
    c_sqlite3_column_int64,
    c_sqlite3_column_double,
    c_sqlite3_column_text,

    -- * Result statistics
    c_sqlite3_last_insert_rowid,
    c_sqlite3_changes,
    c_sqlite3_total_changes,

    -- * Create Or Redefine SQL Functions
    c_sqlite3_create_function_v2,
    CFunc,
    CFuncFinal,
    CFuncDestroy,
    mkCFunc,
    mkCFuncFinal,
    mkCFuncDestroy,
    c_sqlite3_user_data,
    c_sqlite3_context_db_handle,
    c_sqlite3_aggregate_context,

    -- * Obtaining SQL Function Parameter Values
    -- | <http://www.sqlite.org/c3ref/value_blob.html>
    c_sqlite3_value_type,
    c_sqlite3_value_bytes,
    c_sqlite3_value_blob,
    c_sqlite3_value_text,
    c_sqlite3_value_int64,
    c_sqlite3_value_double,

    -- * Setting The Result Of An SQL Function
    -- | <http://www.sqlite.org/c3ref/result_blob.html>
    c_sqlite3_result_null,
    c_sqlite3_result_blob,
    c_sqlite3_result_zeroblob,
    c_sqlite3_result_text,
    c_sqlite3_result_int64,
    c_sqlite3_result_double,
    c_sqlite3_result_value,
    c_sqlite3_result_error,

    -- * Define New Collating Sequences
    c_sqlite3_create_collation_v2,
    CCompare,
    mkCCompare,

    -- * Miscellaneous
    c_sqlite3_free,

    -- * Extensions
    c_sqlite3_enable_load_extension,

    -- * Write-Ahead Log Commit Hook
    c_sqlite3_wal_hook,
    CWalHook,
    mkCWalHook,
) where

import Database.SQLite3.Bindings.Types

import Foreign
import Foreign.C


-- | <http://www.sqlite.org/c3ref/open.html>
--
-- This sets the @'Ptr CDatabase'@ even on failure.
foreign import ccall "sqlite3_open"
    c_sqlite3_open :: CString -> Ptr (Ptr CDatabase) -> IO CError

-- | <http://www.sqlite.org/c3ref/close.html>
foreign import ccall "sqlite3_close"
    c_sqlite3_close :: Ptr CDatabase -> IO CError

-- | <http://www.sqlite.org/c3ref/errcode.html>
foreign import ccall "sqlite3_errmsg"
    c_sqlite3_errmsg :: Ptr CDatabase -> IO CString

-- | <http://www.sqlite.org/c3ref/interrupt.html>
foreign import ccall "sqlite3_interrupt"
    c_sqlite3_interrupt :: Ptr CDatabase -> IO ()

-- | <http://www.sqlite.org/c3ref/profile.html>
foreign import ccall "sqlite3_trace"
    c_sqlite3_trace
        :: Ptr CDatabase
        -> FunPtr (CTraceCallback a) -- ^ Optional callback function called for each row
        -> Ptr a                     -- ^ Context passed to the callback
        -> IO (Ptr ())               -- ^ Returns context pointer from previously
                                     --   registered trace

-- | <http://www.sqlite.org/c3ref/get_autocommit.html>
foreign import ccall unsafe "sqlite3_get_autocommit"
    c_sqlite3_get_autocommit :: Ptr CDatabase -> IO CInt

-- | <https://www.sqlite.org/c3ref/enable_shared_cache.html>
foreign import ccall unsafe "sqlite3_enable_shared_cache"
    c_sqlite3_enable_shared_cache :: CInt -> IO CError


foreign import ccall "sqlite3_exec"
    c_sqlite3_exec
        :: Ptr CDatabase
        -> CString                  -- ^ SQL statement, UTF-8 encoded
        -> FunPtr (CExecCallback a) -- ^ Optional callback function called for each row
        -> Ptr a                    -- ^ Context passed to the callback
        -> Ptr CString              -- ^ OUT: Error message string
        -> IO CError

type CExecCallback a
     = Ptr a
    -> CColumnCount -- ^ Number of columns, which is the number of elements in
                    --   the following arrays.
    -> Ptr CString  -- ^ Array of column values, as returned by
                    --   'c_sqlite3_column_text'.  Null values are represented
                    --   as null pointers.
    -> Ptr CString  -- ^ Array of column names
    -> IO CInt      -- ^ If the callback returns non-zero, then
                    --   'c_sqlite3_exec' returns @SQLITE_ABORT@
                    --   ('ErrorAbort').

type CTraceCallback a
     = Ptr a
    -> CString      -- ^ UTF-8 rendering of the SQL statement text as
                    -- the statement first begins executing
    -> IO ()

-- | A couple important things to know about callbacks from Haskell code:
--
--  * If the callback throws an exception, apparently, the /whole program/ is
--    terminated.
--
--  * Remember to call 'freeHaskellFunPtr' when you are done with the wrapper,
--    to avoid leaking memory.
foreign import ccall "wrapper"
    mkCExecCallback :: CExecCallback a -> IO (FunPtr (CExecCallback a))

foreign import ccall "wrapper"
    mkCTraceCallback :: CTraceCallback a -> IO (FunPtr (CTraceCallback a))


-- | <http://www.sqlite.org/c3ref/prepare.html>
--
-- If the query contains no SQL statements, this returns @SQLITE_OK@ and sets
-- the @'Ptr' 'CStatement'@ to null.
foreign import ccall "sqlite3_prepare_v2"
    c_sqlite3_prepare_v2
        :: Ptr CDatabase
        -> CString              -- ^ SQL statement, UTF-8 encoded
        -> CNumBytes            -- ^ Maximum length of the SQL statement,
                                --   in bytes.  If this is negative, then the
                                --   SQL statement is treated as a
                                --   NUL-terminated string.
        -> Ptr (Ptr CStatement) -- ^ OUT: Statement handle.  This must not be null.
        -> Ptr CString          -- ^ OUT: Pointer to unused portion of zSql
        -> IO CError

-- | <http://www.sqlite.org/c3ref/db_handle.html>
foreign import ccall unsafe "sqlite3_db_handle"
    c_sqlite3_db_handle :: Ptr CStatement -> IO (Ptr CDatabase)

-- | <http://www.sqlite.org/c3ref/step.html>
foreign import ccall "sqlite3_step"
    c_sqlite3_step :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/reset.html>
--
-- /Warning:/ If the most recent 'c_sqlite3_step' call failed,
-- this will return the corresponding error code.
foreign import ccall "sqlite3_reset"
    c_sqlite3_reset :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/finalize.html>
--
-- /Warning:/ If the most recent 'c_sqlite3_step' call failed,
-- this will return the corresponding error code.
foreign import ccall "sqlite3_finalize"
    c_sqlite3_finalize :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/clear_bindings.html>
--
-- A look at the source reveals that this function always returns @SQLITE_OK@.
foreign import ccall unsafe "sqlite3_clear_bindings"
    c_sqlite3_clear_bindings :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/sql.html>
foreign import ccall unsafe "sqlite3_sql"
    c_sqlite3_sql :: Ptr CStatement -> IO CString

-- | <http://www.sqlite.org/c3ref/bind_parameter_count.html>
--
-- This returns the index of the largest (rightmost) parameter, which is not
-- necessarily the number of parameters.  If numbered parameters like @?5@
-- are used, there may be gaps in the list.
foreign import ccall unsafe "sqlite3_bind_parameter_count"
    c_sqlite3_bind_parameter_count :: Ptr CStatement -> IO CParamIndex

-- | <http://www.sqlite.org/c3ref/bind_parameter_name.html>
foreign import ccall unsafe "sqlite3_bind_parameter_name"
    c_sqlite3_bind_parameter_name :: Ptr CStatement -> CParamIndex -> IO CString

-- | <http://www.sqlite.org/c3ref/bind_parameter_index.html>
foreign import ccall unsafe "sqlite3_bind_parameter_index"
    c_sqlite3_bind_parameter_index :: Ptr CStatement -> CString -> IO CParamIndex

-- | <http://www.sqlite.org/c3ref/column_count.html>
foreign import ccall unsafe "sqlite3_column_count"
    c_sqlite3_column_count :: Ptr CStatement -> IO CColumnCount

-- | <http://www.sqlite.org/c3ref/column_name.html>
foreign import ccall unsafe "sqlite3_column_name"
    c_sqlite3_column_name :: Ptr CStatement -> CColumnIndex -> IO CString


foreign import ccall unsafe "sqlite3_bind_blob"
    c_sqlite3_bind_blob
        :: Ptr CStatement
        -> CParamIndex      -- ^ Index of the SQL parameter to be set
        -> Ptr a            -- ^ Value to bind to the parameter.
                            --
                            --   /Warning:/ If this pointer is @NULL@, this
                            --   will bind a null value, rather than an empty blob.
        -> CNumBytes        -- ^ Length, in bytes.  This must not be negative.
        -> Ptr CDestructor
        -> IO CError

foreign import ccall unsafe "sqlite3_bind_zeroblob"
    c_sqlite3_bind_zeroblob
        :: Ptr CStatement -> CParamIndex -> CInt -> IO CError

foreign import ccall unsafe "sqlite3_bind_text"
    c_sqlite3_bind_text
        :: Ptr CStatement
        -> CParamIndex
        -> CString          -- ^ /Warning:/ If this pointer is @NULL@, this
                            --   will bind a null value, rather than an empty text.
        -> CNumBytes        -- ^ Length, in bytes.  If this is negative,
                            --   the value is treated as a NUL-terminated string.
        -> Ptr CDestructor
        -> IO CError

foreign import ccall unsafe "sqlite3_bind_double"
    c_sqlite3_bind_double   :: Ptr CStatement -> CParamIndex -> Double -> IO CError

foreign import ccall unsafe "sqlite3_bind_int64"
    c_sqlite3_bind_int64    :: Ptr CStatement -> CParamIndex -> Int64 -> IO CError

foreign import ccall unsafe "sqlite3_bind_null"
    c_sqlite3_bind_null     :: Ptr CStatement -> CParamIndex -> IO CError


foreign import ccall unsafe "sqlite3_column_type"
    c_sqlite3_column_type   :: Ptr CStatement -> CColumnIndex -> IO CColumnType

foreign import ccall unsafe "sqlite3_column_bytes"
    c_sqlite3_column_bytes  :: Ptr CStatement -> CColumnIndex -> IO CNumBytes

foreign import ccall unsafe "sqlite3_column_blob"
    c_sqlite3_column_blob   :: Ptr CStatement -> CColumnIndex -> IO (Ptr a)

foreign import ccall unsafe "sqlite3_column_text"
    c_sqlite3_column_text   :: Ptr CStatement -> CColumnIndex -> IO CString

foreign import ccall unsafe "sqlite3_column_int64"
    c_sqlite3_column_int64  :: Ptr CStatement -> CColumnIndex -> IO Int64

foreign import ccall unsafe "sqlite3_column_double"
    c_sqlite3_column_double :: Ptr CStatement -> CColumnIndex -> IO Double


-- | <http://www.sqlite.org/c3ref/last_insert_rowid.html>
foreign import ccall unsafe "sqlite3_last_insert_rowid"
    c_sqlite3_last_insert_rowid :: Ptr CDatabase -> IO Int64

-- | <http://www.sqlite.org/c3ref/changes.html>
foreign import ccall unsafe "sqlite3_changes"
    c_sqlite3_changes :: Ptr CDatabase -> IO CInt

-- | <http://www.sqlite.org/c3ref/total_changes.html>
foreign import ccall unsafe "sqlite3_total_changes"
    c_sqlite3_total_changes :: Ptr CDatabase -> IO CInt

-- do not use unsafe import here, it might call back to haskell
-- via the CFuncDestroy argument
-- | <http://sqlite.org/c3ref/create_function.html>
foreign import ccall "sqlite3_create_function_v2"
    c_sqlite3_create_function_v2
        :: Ptr CDatabase
        -> CString         -- ^ Name of the function
        -> CArgCount       -- ^ Number of arguments
        -> CInt            -- ^ Preferred text encoding (also used to pass flags)
        -> Ptr a           -- ^ User data
        -> FunPtr CFunc
        -> FunPtr CFunc
        -> FunPtr CFuncFinal
        -> FunPtr (CFuncDestroy a)
        -> IO CError

type CFunc          = Ptr CContext -> CArgCount -> Ptr (Ptr CValue) -> IO ()

type CFuncFinal     = Ptr CContext -> IO ()

type CFuncDestroy a = Ptr a -> IO ()

foreign import ccall "wrapper"
    mkCFunc        :: CFunc          -> IO (FunPtr CFunc)

foreign import ccall "wrapper"
    mkCFuncFinal   :: CFuncFinal     -> IO (FunPtr CFuncFinal)

foreign import ccall "wrapper"
    mkCFuncDestroy :: CFuncDestroy a -> IO (FunPtr (CFuncDestroy a))

-- | <http://www.sqlite.org/c3ref/user_data.html>
foreign import ccall unsafe "sqlite3_user_data"
    c_sqlite3_user_data :: Ptr CContext -> IO (Ptr a)

-- | <http://www.sqlite.org/c3ref/context_db_handle.html>
foreign import ccall unsafe "sqlite3_context_db_handle"
    c_sqlite3_context_db_handle :: Ptr CContext -> IO (Ptr CDatabase)

-- | <http://www.sqlite.org/c3ref/aggregate_context.html>
foreign import ccall unsafe "sqlite3_aggregate_context"
    c_sqlite3_aggregate_context :: Ptr CContext -> CNumBytes -> IO (Ptr a)


foreign import ccall unsafe "sqlite3_value_type"
    c_sqlite3_value_type   :: Ptr CValue -> IO CColumnType

foreign import ccall unsafe "sqlite3_value_bytes"
    c_sqlite3_value_bytes  :: Ptr CValue -> IO CNumBytes

foreign import ccall unsafe "sqlite3_value_blob"
    c_sqlite3_value_blob   :: Ptr CValue -> IO (Ptr a)

foreign import ccall unsafe "sqlite3_value_text"
    c_sqlite3_value_text   :: Ptr CValue -> IO CString

foreign import ccall unsafe "sqlite3_value_int64"
    c_sqlite3_value_int64  :: Ptr CValue -> IO Int64

foreign import ccall unsafe "sqlite3_value_double"
    c_sqlite3_value_double :: Ptr CValue -> IO Double


foreign import ccall unsafe "sqlite3_result_null"
    c_sqlite3_result_null     :: Ptr CContext -> IO ()

foreign import ccall unsafe "sqlite3_result_blob"
    c_sqlite3_result_blob     :: Ptr CContext -> Ptr a -> CNumBytes -> Ptr CDestructor -> IO ()

foreign import ccall unsafe "sqlite3_result_zeroblob"
    c_sqlite3_result_zeroblob :: Ptr CContext -> CNumBytes -> IO ()

foreign import ccall unsafe "sqlite3_result_text"
    c_sqlite3_result_text     :: Ptr CContext -> CString -> CNumBytes -> Ptr CDestructor -> IO ()

foreign import ccall unsafe "sqlite3_result_int64"
    c_sqlite3_result_int64    :: Ptr CContext -> Int64 -> IO ()

foreign import ccall unsafe "sqlite3_result_double"
    c_sqlite3_result_double   :: Ptr CContext -> Double -> IO ()

foreign import ccall unsafe "sqlite3_result_value"
    c_sqlite3_result_value    :: Ptr CContext -> Ptr CValue -> IO ()

foreign import ccall unsafe "sqlite3_result_error"
    c_sqlite3_result_error    :: Ptr CContext -> CString -> CNumBytes -> IO ()


-- | <http://www.sqlite.org/c3ref/create_collation.html>
foreign import ccall "sqlite3_create_collation_v2"
    c_sqlite3_create_collation_v2
        :: Ptr CDatabase
        -> CString         -- ^ Name of the collation
        -> CInt            -- ^ Text encoding
        -> Ptr a           -- ^ User data
        -> FunPtr (CCompare a)
        -> FunPtr (CFuncDestroy a)
        -> IO CError

type CCompare a = Ptr a -> CNumBytes -> CString -> CNumBytes -> CString -> IO CInt

foreign import ccall "wrapper"
    mkCCompare :: CCompare a -> IO (FunPtr (CCompare a))


-- | <http://sqlite.org/c3ref/free.html>
foreign import ccall "sqlite3_free"
    c_sqlite3_free :: Ptr a -> IO ()


-- | <http://sqlite.org/c3ref/enable_load_extension.html>
foreign import ccall "sqlite3_enable_load_extension"
    c_sqlite3_enable_load_extension :: Ptr CDatabase -> Bool -> IO CError


-- | <https://www.sqlite.org/c3ref/wal_hook.html>
foreign import ccall unsafe "sqlite3_wal_hook"
    c_sqlite3_wal_hook :: Ptr CDatabase -> FunPtr CWalHook -> Ptr a -> IO (Ptr ())

type CWalHook = Ptr () -> Ptr CDatabase -> CString -> CInt -> IO CError

foreign import ccall "wrapper"
    mkCWalHook :: CWalHook -> IO (FunPtr CWalHook)
