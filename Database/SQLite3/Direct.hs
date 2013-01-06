{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- This API is a slightly lower-level version of "Database.SQLite3".  Namely:
--
--  * It returns errors instead of throwing them.
--
--  * It only uses cheap conversions.  None of these bindings convert from
--    'String' or 'T.Text'.
module Database.SQLite3.Direct (
    -- * Connection management
    open,
    close,
    errmsg,
    setTrace,

    -- * Simple query execution
    -- | <http://sqlite.org/c3ref/exec.html>
    exec,
    execWithCallback,
    ExecCallback,

    -- * Statement management
    prepare,
    getStatementDatabase,
    step,
    reset,
    finalize,
    clearBindings,
    statementText,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    columnCount,

    -- * Binding values to a prepared statement
    -- | <http://www.sqlite.org/c3ref/bind_blob.html>
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindNull,

    -- * Reading the result row
    -- | <http://www.sqlite.org/c3ref/column_blob.html>
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * Result statistics
    lastInsertRowId,
    changes,
    totalChanges,

    -- * Interrupting a long-running query
    interrupt,

    -- * Types
    Database(..),
    Statement(..),
    ColumnType(..),

    -- ** Results and errors
    StepResult(..),
    Error(..),

    -- ** Special types
    Utf8(..),
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,
) where

import Database.SQLite3.Bindings

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Unsafe     as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative  ((<$>))
import Control.Exception as E
import Control.Monad        (join)
import Data.ByteString      (ByteString)
import Data.IORef
import Data.Monoid
import Data.String          (IsString(..))
import Data.Text.Encoding.Error (lenientDecode)
import Foreign
import Foreign.C

newtype Database = Database (Ptr CDatabase)
    deriving (Eq, Show)

newtype Statement = Statement (Ptr CStatement)
    deriving (Eq, Show)

data StepResult
    = Row
    | Done
    deriving (Eq, Show)

-- | A 'ByteString' containing UTF8-encoded text with no NUL characters.
newtype Utf8 = Utf8 ByteString
    deriving (Eq, Ord)

instance Show Utf8 where
    show (Utf8 s) = (show . T.decodeUtf8With lenientDecode) s

-- | @fromString = Utf8 . 'T.encodeUtf8' . 'T.pack'@
instance IsString Utf8 where
    fromString = Utf8 . T.encodeUtf8 . T.pack

instance Monoid Utf8 where
    mempty = Utf8 BS.empty
    mappend (Utf8 a) (Utf8 b) = Utf8 (BS.append a b)
    mconcat = Utf8 . BS.concat . map (\(Utf8 s) -> s)

packUtf8 :: a -> (Utf8 -> a) -> CString -> IO a
packUtf8 n f cstr | cstr == nullPtr = return n
                  | otherwise       = f . Utf8 <$> BS.packCString cstr

packCStringLen :: CString -> CNumBytes -> IO ByteString
packCStringLen cstr len =
    BS.packCStringLen (cstr, fromIntegral len)

packUtf8Array :: IO a -> (Utf8 -> IO a) -> Int -> Ptr CString -> IO [a]
packUtf8Array onNull onUtf8 count base =
    peekArray count base >>= mapM (join . packUtf8 onNull onUtf8)

-- | Like 'unsafeUseAsCStringLen', but if the string is empty,
-- never pass the callback a null pointer.
unsafeUseAsCStringLenNoNull :: ByteString -> (CString -> CNumBytes -> IO a) -> IO a
unsafeUseAsCStringLenNoNull bs cb
    | BS.null bs = cb (intPtrToPtr 1) 0
    | otherwise  = BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
                       cb ptr (fromIntegral len)

wrapNullablePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
wrapNullablePtr f ptr | ptr == nullPtr = Nothing
                      | otherwise      = Just (f ptr)

type Result a = Either Error a

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

-- | <http://www.sqlite.org/c3ref/open.html>
open :: Utf8 -> IO (Either (Error, Utf8) Database)
open (Utf8 path) =
    BS.useAsCString path $ \path' ->
    alloca $ \database -> do
        rc <- c_sqlite3_open path' database
        db <- Database <$> peek database
            -- sqlite3_open returns a sqlite3 even on failure.
            -- That's where we get a more descriptive error message.
        case toResult () rc of
            Left err -> do
                msg <- errmsg db -- This returns "out of memory" if db is null.
                _   <- close db  -- This is harmless if db is null.
                return $ Left (err, msg)
            Right () ->
                if db == Database nullPtr
                    then fail "sqlite3_open unexpectedly returned NULL"
                    else return $ Right db

-- | <http://www.sqlite.org/c3ref/close.html>
close :: Database -> IO (Either Error ())
close (Database db) =
    toResult () <$> c_sqlite3_close db

-- | <http://www.sqlite.org/c3ref/interrupt.html>
--
-- Cause any pending operation on the 'Database' handle to stop at its earliest
-- opportunity.  This simply sets a flag and returns immediately.  It does not
-- wait for the pending operation to finish.
--
-- You'll need to compile with @-threaded@ for this to do any good.
-- Without @-threaded@, FFI calls block the whole RTS, meaning 'interrupt'
-- would never run at the same time as 'step'.
interrupt :: Database -> IO ()
interrupt (Database db) =
    c_sqlite3_interrupt db

-- | <http://www.sqlite.org/c3ref/errcode.html>
errmsg :: Database -> IO Utf8
errmsg (Database db) =
    c_sqlite3_errmsg db >>= packUtf8 (Utf8 BS.empty) id

exec :: Database -> Utf8 -> IO (Either (Error, Utf8) ())
exec (Database db) (Utf8 sql) =
    BS.useAsCString sql $ \sql' ->
    alloca $ \msgPtrOut -> do
        rc <- c_sqlite3_exec db sql' nullFunPtr nullPtr msgPtrOut
        case toResult () rc of
            Left err -> do
                msgPtr <- peek msgPtrOut
                msg <- packUtf8 (Utf8 BS.empty) id msgPtr
                c_sqlite3_free msgPtr
                return $ Left (err, msg)
            Right () -> return $ Right ()

-- | Like 'exec', but invoke the callback for each result row.
--
-- If the callback throws an exception, it will be rethrown by
-- 'execWithCallback'.
execWithCallback :: Database -> Utf8 -> ExecCallback -> IO (Either (Error, Utf8) ())
execWithCallback (Database db) (Utf8 sql) cb = do
    abortReason <- newIORef Nothing :: IO (IORef (Maybe SomeException))
    cbCache <- newIORef Nothing :: IO (IORef (Maybe ([Maybe Utf8] -> IO ())))
        -- Cache the partial application of column count and name, so if the
        -- caller wants to convert them to something else, it only has to do
        -- the conversions once.

    let getCallback cCount cNames = do
            m <- readIORef cbCache
            case m of
                Nothing -> do
                    names <- packUtf8Array (fail "execWithCallback: NULL column name")
                                           return
                                           (fromIntegral cCount) cNames
                    let !cb' = cb (fromFFI cCount) names
                    writeIORef cbCache $ Just cb'
                    return cb'
                Just cb' -> return cb'

    let onExceptionAbort io =
          (io >> return 0) `E.catch` \ex -> do
            writeIORef abortReason $ Just ex
            return 1

    let cExecCallback _ctx cCount cValues cNames =
          onExceptionAbort $ do
            cb' <- getCallback cCount cNames
            values <- packUtf8Array (return Nothing)
                                    (return . Just)
                                    (fromIntegral cCount) cValues
            cb' values

    BS.useAsCString sql $ \sql' ->
      alloca $ \msgPtrOut ->
      bracket (mkCExecCallback cExecCallback) freeHaskellFunPtr $
      \pExecCallback -> do
        let returnError err = do
                msgPtr <- peek msgPtrOut
                msg <- packUtf8 (Utf8 BS.empty) id msgPtr
                c_sqlite3_free msgPtr
                return $ Left (err, msg)
        rc <- c_sqlite3_exec db sql' pExecCallback nullPtr msgPtrOut
        case toResult () rc of
            Left ErrorAbort -> do
                m <- readIORef abortReason
                case m of
                    Nothing -> returnError ErrorAbort
                    Just ex -> throwIO ex
            Left err -> returnError err
            Right () -> return $ Right ()

type ExecCallback
     = ColumnCount    -- ^ Number of columns, which is the number of items in
                      --   the following lists.  This will be the same for
                      --   every row.
    -> [Utf8]         -- ^ List of column names.  This will be the same
                      --   for every row.
    -> [Maybe Utf8]   -- ^ List of column values, as returned by 'columnText'.
    -> IO ()

-- | <http://www.sqlite.org/c3ref/profile.html>
--
-- Enable/disable tracing of SQL execution.  Tracing can be disabled
-- by setting 'Nothing' as the logger callback.
--
-- Warning: If the logger callback throws an exception, your whole
-- program will crash.  Enable only for debugging!
setTrace :: Database -> Maybe (Utf8 -> IO ()) -> IO ()
setTrace (Database db) logger =
    case logger of
        Nothing -> do
            _ <- c_sqlite3_trace db nullFunPtr nullPtr
            return ()
        Just output -> do
            -- NB: this FunPtr never gets freed.  Shouldn't be a big deal,
            -- though, since 'setTrace' is mainly for debugging, and is
            -- typically only called once per application invocation.
            cb <- mkCTraceCallback $ \_ctx cStr -> do
                msg <- packUtf8 (Utf8 BS.empty) id cStr
                output msg
            _ <- c_sqlite3_trace db cb nullPtr
            return ()

-- | <http://www.sqlite.org/c3ref/prepare.html>
--
-- If the query contains no SQL statements, this returns
-- @'Right' 'Nothing'@.
prepare :: Database -> Utf8 -> IO (Either Error (Maybe Statement))
prepare (Database db) (Utf8 sql) =
    BS.useAsCString sql $ \sql' ->
        alloca $ \statement ->
            c_sqlite3_prepare_v2 db sql' (-1) statement nullPtr >>=
                toResultM (wrapNullablePtr Statement <$> peek statement)

-- | <http://www.sqlite.org/c3ref/db_handle.html>
getStatementDatabase :: Statement -> IO Database
getStatementDatabase (Statement stmt) = do
    db <- c_sqlite3_db_handle stmt
    if db == nullPtr
        then fail $ "sqlite3_db_handle(" ++ show stmt ++ ") returned NULL"
        else return (Database db)

-- | <http://www.sqlite.org/c3ref/step.html>
step :: Statement -> IO (Either Error StepResult)
step (Statement stmt) =
    toStepResult <$> c_sqlite3_step stmt

-- | <http://www.sqlite.org/c3ref/reset.html>
--
-- Warning:
--
--  * If the most recent 'step' call failed,
--    this will return the corresponding error.
--
--  * This does not reset the bindings on a prepared statement.
--    Use 'clearBindings' to do that.
reset :: Statement -> IO (Either Error ())
reset (Statement stmt) =
    toResult () <$> c_sqlite3_reset stmt

-- | <http://www.sqlite.org/c3ref/finalize.html>
--
-- /Warning:/ If the most recent 'step' call failed,
-- this will return the corresponding error.
finalize :: Statement -> IO (Either Error ())
finalize (Statement stmt) =
    toResult () <$> c_sqlite3_finalize stmt

-- | <http://www.sqlite.org/c3ref/sql.html>
--
-- Return a copy of the original SQL text used to compile the statement.
statementText :: Statement -> IO (Maybe Utf8)
statementText (Statement stmt) =
    c_sqlite3_sql stmt >>= packUtf8 Nothing Just

-- | <http://www.sqlite.org/c3ref/clear_bindings.html>
--
-- Set all parameters in the prepared statement to null.
clearBindings :: Statement -> IO ()
clearBindings (Statement stmt) = do
    _ <- c_sqlite3_clear_bindings stmt
    return ()

-- | <http://www.sqlite.org/c3ref/bind_parameter_count.html>
--
-- This returns the index of the largest (rightmost) parameter.  Note that this
-- is not necessarily the number of parameters.  If numbered parameters like
-- @?5@ are used, there may be gaps in the list.
--
-- See 'ParamIndex' for more information.
bindParameterCount :: Statement -> IO ParamIndex
bindParameterCount (Statement stmt) =
    fromFFI <$> c_sqlite3_bind_parameter_count stmt

-- | <http://www.sqlite.org/c3ref/bind_parameter_name.html>
bindParameterName :: Statement -> ParamIndex -> IO (Maybe Utf8)
bindParameterName (Statement stmt) idx =
    c_sqlite3_bind_parameter_name stmt (toFFI idx) >>=
        packUtf8 Nothing Just

-- | <http://www.sqlite.org/c3ref/column_count.html>
columnCount :: Statement -> IO ColumnCount
columnCount (Statement stmt) =
    fromFFI <$> c_sqlite3_column_count stmt

bindInt64 :: Statement -> ParamIndex -> Int64 -> IO (Either Error ())
bindInt64 (Statement stmt) idx value =
    toResult () <$> c_sqlite3_bind_int64 stmt (toFFI idx) value

bindDouble :: Statement -> ParamIndex -> Double -> IO (Either Error ())
bindDouble (Statement stmt) idx value =
    toResult () <$> c_sqlite3_bind_double stmt (toFFI idx) value

bindText :: Statement -> ParamIndex -> Utf8 -> IO (Either Error ())
bindText (Statement stmt) idx (Utf8 value) =
    unsafeUseAsCStringLenNoNull value $ \ptr len ->
        toResult () <$>
            c_sqlite3_bind_text stmt (toFFI idx) ptr len c_SQLITE_TRANSIENT

bindBlob :: Statement -> ParamIndex -> ByteString -> IO (Either Error ())
bindBlob (Statement stmt) idx value =
    unsafeUseAsCStringLenNoNull value $ \ptr len ->
        toResult () <$>
            c_sqlite3_bind_blob stmt (toFFI idx) ptr len c_SQLITE_TRANSIENT

bindNull :: Statement -> ParamIndex -> IO (Either Error ())
bindNull (Statement stmt) idx =
    toResult () <$> c_sqlite3_bind_null stmt (toFFI idx)

columnType :: Statement -> ColumnIndex -> IO ColumnType
columnType (Statement stmt) idx =
    decodeColumnType <$> c_sqlite3_column_type stmt (toFFI idx)

columnInt64 :: Statement -> ColumnIndex -> IO Int64
columnInt64 (Statement stmt) idx =
    c_sqlite3_column_int64 stmt (toFFI idx)

columnDouble :: Statement -> ColumnIndex -> IO Double
columnDouble (Statement stmt) idx =
    c_sqlite3_column_double stmt (toFFI idx)

columnText :: Statement -> ColumnIndex -> IO Utf8
columnText (Statement stmt) idx = do
    ptr <- c_sqlite3_column_text stmt (toFFI idx)
    len <- c_sqlite3_column_bytes stmt (toFFI idx)
    Utf8 <$> packCStringLen ptr len

columnBlob :: Statement -> ColumnIndex -> IO ByteString
columnBlob (Statement stmt) idx = do
    ptr <- c_sqlite3_column_blob stmt (toFFI idx)
    len <- c_sqlite3_column_bytes stmt (toFFI idx)
    packCStringLen ptr len


-- | <http://www.sqlite.org/c3ref/last_insert_rowid.html>
lastInsertRowId :: Database -> IO Int64
lastInsertRowId (Database db) =
    c_sqlite3_last_insert_rowid db

-- | <http://www.sqlite.org/c3ref/changes.html>
--
-- Return the number of rows that were changed, inserted, or deleted
-- by the most recent @INSERT@, @DELETE@, or @UPDATE@ statement.
changes :: Database -> IO Int
changes (Database db) =
    fromIntegral <$> c_sqlite3_changes db

-- | <http://www.sqlite.org/c3ref/total_changes.html>
--
-- Return the total number of row changes caused by @INSERT@, @DELETE@,
-- or @UPDATE@ statements since the 'Database' was opened.
totalChanges :: Database -> IO Int
totalChanges (Database db) =
    fromIntegral <$> c_sqlite3_total_changes db
