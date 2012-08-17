{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Database.SQLite3 (
                         Database,
                         Statement,
                         Error(..),
                         StepResult(Row,
                                    Done),
                         SQLData(SQLInteger,
                                 SQLFloat,
                                 SQLText,
                                 SQLBlob,
                                 SQLNull),
                         open,
                         close,
                         prepare,
                         step,
                         reset,
                         finalize,
                         bindParameterCount,
                         bindParameterName,
                         bindBlob,
                         bindDouble,
                         bindInt,
                         bindInt64,
                         bindNull,
                         bindText,
                         bind,
                         column,
                         columns
                        )
    where

import Database.SQLite3.Bindings
    hiding (decodeError, decodeColumnType)

import qualified Database.SQLite3.Bindings as Bindings
    (decodeError, decodeColumnType)

import Prelude hiding (error)
import qualified Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Foreign
import Foreign.C


newtype Database  = Database  (Ptr CDatabase)
newtype Statement = Statement (Ptr CStatement)

decodeError :: CError -> Error
decodeError = wrapDecode "decodeError" Bindings.decodeError

decodeColumnType :: CColumnType -> ColumnType
decodeColumnType = wrapDecode "decodeColumnType" Bindings.decodeColumnType

wrapDecode :: Show a => String -> (a -> Maybe b) -> a -> b
wrapDecode funcName f a =
    case f a of
        Just x  -> x
        Nothing -> Prelude.error $ funcName ++ " " ++ show a

data StepResult = Row | Done deriving (Eq, Show)

data SQLData = SQLInteger Int64
             | SQLFloat Double
             | SQLText T.Text
             | SQLBlob BS.ByteString
             | SQLNull
               deriving (Eq, Show, Typeable)


foreign import ccall "sqlite3_errmsg"
  errmsgC :: Ptr CDatabase -> IO CString
errmsg :: Database -> IO String
errmsg (Database database) = do
  message <- errmsgC database
  byteString <- BS.packCString message
  return $ T.unpack $ T.decodeUtf8 byteString

sqlError :: Maybe Database -> String -> Error -> IO a
sqlError maybeDatabase functionName error = do
  details <- case maybeDatabase of
               Just database -> do
                 details <- errmsg database
                 return $ ": " ++ details
               Nothing -> return "."
  fail $ "SQLite3 returned " ++ (show error)
         ++ " while attempting to perform " ++ functionName
         ++ details

foreign import ccall "sqlite3_open"
  openC :: CString -> Ptr (Ptr CDatabase) -> IO CError
openError :: String -> IO (Either Database Error)
openError path = do
  BS.useAsCString (T.encodeUtf8 $ T.pack path)
                  (\path -> do
                     alloca (\database -> do
                               error <- openC path database
                               error <- return $ decodeError error
                               case error of
                                 ErrorOK -> do
                                            database <- peek database
                                            return $ Left $ Database database
                                 _ -> return $ Right error))
open :: String -> IO Database
open path = do
  databaseOrError <- openError path
  case databaseOrError of
    Left database -> return database
    Right error -> sqlError Nothing ("open " ++ show path) error

foreign import ccall "sqlite3_close"
  closeC :: Ptr CDatabase -> IO CError
closeError :: Database -> IO Error
closeError (Database database) = do
  error <- closeC database
  return $ decodeError error
close :: Database -> IO ()
close database = do
  error <- closeError database
  case error of
    ErrorOK -> return ()
    _ -> sqlError (Just database) "close" error

foreign import ccall "sqlite3_prepare_v2"
  prepareC :: Ptr CDatabase         -- ^ Database handle
           -> CString               -- ^ SQL statement, UTF-8 encoded
           -> Int                   -- ^ Maximum length of zSql in bytes.
           -> Ptr (Ptr CStatement)  -- ^ OUT: Statement handle
           -> Ptr CString           -- ^ OUT: Pointer to unused portion of zSql
           -> IO CError
prepareError :: Database -> String -> IO (Either Statement Error)
prepareError (Database database) text = do
  BS.useAsCString (T.encodeUtf8 $ T.pack text)
                  (\text -> do
                     alloca (\statement -> do
                               error <- prepareC database text (-1) statement nullPtr
                               error <- return $ decodeError error
                               case error of
                                 ErrorOK -> do
                                            statement <- peek statement
                                            return $ Left $ Statement statement
                                 _ -> return $ Right error))
prepare :: Database -> String -> IO Statement
prepare database text = do
  statementOrError <- prepareError database text
  case statementOrError of
    Left statement -> return statement
    Right error -> sqlError (Just database) ("prepare " ++ (show text)) error

foreign import ccall "sqlite3_step"
  stepC :: Ptr CStatement -> IO CError
stepError :: Statement -> IO Error
stepError (Statement statement) = do
  error <- stepC statement
  return $ decodeError error
step :: Statement -> IO StepResult
step statement = do
  error <- stepError statement
  case error of
    ErrorRow -> return Row
    ErrorDone -> return Done
    _ -> sqlError Nothing "step" error

foreign import ccall "sqlite3_reset"
  resetC :: Ptr CStatement -> IO CError
resetError :: Statement -> IO Error
resetError (Statement statement) = do
  error <- resetC statement
  return $ decodeError error
reset :: Statement -> IO ()
reset statement = do
  error <- resetError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "reset" error

foreign import ccall "sqlite3_finalize"
  finalizeC :: Ptr CStatement -> IO CError
finalizeError :: Statement -> IO Error
finalizeError (Statement statement) = do
  error <- finalizeC statement
  return $ decodeError error
finalize :: Statement -> IO ()
finalize statement = do
  error <- finalizeError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "finalize" error


foreign import ccall "sqlite3_bind_parameter_count"
  bindParameterCountC :: Ptr CStatement -> IO Int

-- | Find the number SQL parameters in a prepared statement.
bindParameterCount :: Statement -> IO Int
bindParameterCount (Statement stmt) = do
  bindParameterCountC stmt

maybeNullCString :: CString -> IO (Maybe BS.ByteString)
maybeNullCString s =
  if s == nullPtr then return Nothing else fmap Just (BS.packCString s)

foreign import ccall "sqlite3_bind_parameter_name"
  bindParameterNameC :: Ptr CStatement -> Int -> IO CString

-- | Return the N-th SQL parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the column index starts at 1, not 0.
bindParameterName :: Statement -> Int -> IO (Maybe String)
bindParameterName (Statement stmt) colNdx = do
  mn <- bindParameterNameC stmt colNdx >>= maybeNullCString
  return (mn >>= return . T.unpack . T.decodeUtf8)

foreign import ccall "sqlite3_bind_blob"
  bindBlobC :: Ptr CStatement
            -> Int              -- ^ Index of the SQL parameter to be set
            -> Ptr ()           -- ^ Value to bind to the parameter.
                                --   C type: void *ptr
            -> Int              -- ^ Length, in bytes
            -> Ptr CDestructor
            -> IO CError
bindBlobError :: Statement -> Int -> BS.ByteString -> IO Error
bindBlobError (Statement statement) parameterIndex byteString = do
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\dataC -> do
                     error <- bindBlobC statement parameterIndex (castPtr dataC) size
                                        c_SQLITE_TRANSIENT
                     return $ decodeError error)
bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString = do
  error <- bindBlobError statement parameterIndex byteString
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind blob" error

foreign import ccall "sqlite3_bind_double"
  bindDoubleC :: Ptr CStatement -> Int -> Double -> IO CError
bindDoubleError :: Statement -> Int -> Double -> IO Error
bindDoubleError (Statement statement) parameterIndex datum = do
  error <- bindDoubleC statement parameterIndex datum
  return $ decodeError error
bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble statement parameterIndex datum = do
  error <- bindDoubleError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind double" error

foreign import ccall "sqlite3_bind_int"
  bindIntC :: Ptr CStatement -> Int -> Int -> IO CError
bindIntError :: Statement -> Int -> Int -> IO Error
bindIntError (Statement statement) parameterIndex datum = do
  error <- bindIntC statement parameterIndex datum
  return $ decodeError error
bindInt :: Statement -> Int -> Int -> IO ()
bindInt statement parameterIndex datum = do
  error <- bindIntError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int" error

foreign import ccall "sqlite3_bind_int64"
  bindInt64C :: Ptr CStatement -> Int -> Int64 -> IO CError
bindInt64Error :: Statement -> Int -> Int64 -> IO Error
bindInt64Error (Statement statement) parameterIndex datum = do
  error <- bindInt64C statement parameterIndex datum
  return $ decodeError error
bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 statement parameterIndex datum = do
  error <- bindInt64Error statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int64" error

foreign import ccall "sqlite3_bind_null"
  bindNullC :: Ptr CStatement -> Int -> IO CError
bindNullError :: Statement -> Int -> IO Error
bindNullError (Statement statement) parameterIndex = do
  error <- bindNullC statement parameterIndex
  return $ decodeError error
bindNull :: Statement -> Int -> IO ()
bindNull statement parameterIndex = do
  error <- bindNullError statement parameterIndex
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind null" error

foreign import ccall "sqlite3_bind_text"
  bindTextC :: Ptr CStatement -> Int -> CString -> Int -> Ptr CDestructor -> IO CError
bindTextError :: Statement -> Int -> T.Text -> IO Error
bindTextError (Statement statement) parameterIndex text = do
  byteString <- return $ T.encodeUtf8 text
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\dataC -> do
                     error <- bindTextC statement parameterIndex dataC size
                                        c_SQLITE_TRANSIENT
                     return $ decodeError error)
bindText :: Statement -> Int -> T.Text -> IO ()
bindText statement parameterIndex text = do
  error <- bindTextError statement parameterIndex text
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind text" error

bind :: Statement -> [SQLData] -> IO ()
bind statement sqlData = do
  mapM_ (\(parameterIndex, datum) -> do
          case datum of
            SQLInteger int64 -> bindInt64 statement parameterIndex int64
            SQLFloat double -> bindDouble statement parameterIndex double
            SQLText text -> bindText statement parameterIndex text
            SQLBlob blob -> bindBlob statement parameterIndex blob
            SQLNull -> bindNull statement parameterIndex)
       $ zip [1..] sqlData

foreign import ccall "sqlite3_column_type"
  columnTypeC :: Ptr CStatement -> Int -> IO CColumnType
columnType :: Statement -> Int -> IO ColumnType
columnType (Statement statement) columnIndex = do
  result <- columnTypeC statement columnIndex
  return $ decodeColumnType result

foreign import ccall "sqlite3_column_bytes"
  columnBytesC :: Ptr CStatement -> Int -> IO Int

foreign import ccall "sqlite3_column_blob"
  columnBlobC :: Ptr CStatement -> Int -> IO (Ptr ())
columnBlob :: Statement -> Int -> IO BS.ByteString
columnBlob (Statement statement) columnIndex = do
  size <- columnBytesC statement columnIndex
  BSI.create size (\resultPtr -> do
                     dataPtr <- columnBlobC statement columnIndex
                     if dataPtr /= nullPtr
                        then BSI.memcpy resultPtr (castPtr dataPtr) (fromIntegral size)
                        else return ())

foreign import ccall "sqlite3_column_int64"
  columnInt64C :: Ptr CStatement -> Int -> IO Int64
columnInt64 :: Statement -> Int -> IO Int64
columnInt64 (Statement statement) columnIndex = do
  columnInt64C statement columnIndex

foreign import ccall "sqlite3_column_double"
  columnDoubleC :: Ptr CStatement -> Int -> IO Double
columnDouble :: Statement -> Int -> IO Double
columnDouble (Statement statement) columnIndex = do
  columnDoubleC statement columnIndex

foreign import ccall "sqlite3_column_text"
  columnTextC :: Ptr CStatement -> Int -> IO CString
columnText :: Statement -> Int -> IO T.Text
columnText (Statement statement) columnIndex = do
  text <- columnTextC statement columnIndex
  byteString <- BS.packCString text
  return $ T.decodeUtf8 byteString

foreign import ccall "sqlite3_column_count"
  columnCountC :: Ptr CStatement -> IO Int
columnCount :: Statement -> IO Int
columnCount (Statement statement) = do
  columnCountC statement

column :: Statement -> Int -> IO SQLData
column statement columnIndex = do
  theType <- columnType statement columnIndex
  case theType of
    IntegerColumn -> do
                 int64 <- columnInt64 statement columnIndex
                 return $ SQLInteger int64
    FloatColumn -> do
                 double <- columnDouble statement columnIndex
                 return $ SQLFloat double
    TextColumn -> do
                 text <- columnText statement columnIndex
                 return $ SQLText text
    BlobColumn -> do
                 byteString <- columnBlob statement columnIndex
                 return $ SQLBlob byteString
    NullColumn -> return SQLNull

columns :: Statement -> IO [SQLData]
columns statement = do
  count <- columnCount statement
  mapM (\i -> column statement i) [0..count-1]
