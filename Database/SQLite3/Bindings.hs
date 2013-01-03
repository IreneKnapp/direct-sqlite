{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite3.Bindings (
    module Database.SQLite3.Bindings.Types,

    -- * Connection management
    c_sqlite3_open,
    c_sqlite3_close,
    c_sqlite3_errmsg,
    c_sqlite3_interrupt,

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

    -- * Parameter and column information
    c_sqlite3_bind_parameter_count,
    c_sqlite3_bind_parameter_name,
    c_sqlite3_column_count,

    -- * Binding Values To Prepared Statements
    -- | <http://www.sqlite.org/c3ref/bind_blob.html>
    c_sqlite3_bind_blob,
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

    -- * Miscellaneous
    c_sqlite3_free,
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

-- | A couple important things to know about callbacks from Haskell code:
--
--  * If the callback throws an exception, apparently, the /whole program/ is
--    terminated.
--
--  * Remember to call 'freeHaskellFunPtr' when you are done with the wrapper,
--    to avoid leaking memory.
foreign import ccall "wrapper"
    mkCExecCallback :: CExecCallback a -> IO (FunPtr (CExecCallback a))


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

-- | <http://www.sqlite.org/c3ref/column_count.html>
foreign import ccall unsafe "sqlite3_column_count"
    c_sqlite3_column_count :: Ptr CStatement -> IO CColumnCount


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


-- | <http://sqlite.org/c3ref/free.html>
foreign import ccall "sqlite3_free"
    c_sqlite3_free :: Ptr a -> IO ()
