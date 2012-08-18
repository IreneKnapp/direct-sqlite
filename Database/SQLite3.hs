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


newtype Database  = Database  (Ptr CDatabase)
newtype Statement = Statement (Ptr CStatement)

data StepResult = Row | Done deriving (Eq, Show)

data SQLData = SQLInteger Int64
             | SQLFloat Double
             | SQLText T.Text
             | SQLBlob BS.ByteString
             | SQLNull
               deriving (Eq, Show, Typeable)


errmsg :: Database -> IO String
errmsg (Database database) = do
  message <- c_sqlite3_errmsg database
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

openError :: String -> IO (Either Database Error)
openError path = do
  BS.useAsCString (T.encodeUtf8 $ T.pack path)
                  (\path -> do
                     alloca (\database -> do
                               error <- c_sqlite3_open path database
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

closeError :: Database -> IO Error
closeError (Database database) = do
  error <- c_sqlite3_close database
  return $ decodeError error
close :: Database -> IO ()
close database = do
  error <- closeError database
  case error of
    ErrorOK -> return ()
    _ -> sqlError (Just database) "close" error

prepareError :: Database -> String -> IO (Either Statement Error)
prepareError (Database database) text = do
  BS.useAsCString (T.encodeUtf8 $ T.pack text)
                  (\text -> do
                     alloca (\statement -> do
                               error <- c_sqlite3_prepare_v2 database
                                                             text
                                                             (CNumBytes (-1))
                                                             statement
                                                             nullPtr
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

stepError :: Statement -> IO Error
stepError (Statement statement) = do
  error <- c_sqlite3_step statement
  return $ decodeError error
step :: Statement -> IO StepResult
step statement = do
  error <- stepError statement
  case error of
    ErrorRow -> return Row
    ErrorDone -> return Done
    _ -> sqlError Nothing "step" error

resetError :: Statement -> IO Error
resetError (Statement statement) = do
  error <- c_sqlite3_reset statement
  return $ decodeError error
reset :: Statement -> IO ()
reset statement = do
  error <- resetError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "reset" error

finalizeError :: Statement -> IO Error
finalizeError (Statement statement) = do
  error <- c_sqlite3_finalize statement
  return $ decodeError error
finalize :: Statement -> IO ()
finalize statement = do
  error <- finalizeError statement
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "finalize" error


-- | This returns the index of the largest (rightmost) parameter, which is not
-- necessarily the number of parameters.  If numbered parameters like @?5@ are
-- used, there may be gaps in the list.
bindParameterCount :: Statement -> IO Int
bindParameterCount (Statement stmt) =
  fromParamIndex <$> c_sqlite3_bind_parameter_count stmt

maybeNullCString :: CString -> IO (Maybe BS.ByteString)
maybeNullCString s =
  if s == nullPtr then return Nothing else Just <$> BS.packCString s

-- | Return the N-th SQL parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the parameter index starts at 1, not 0.
bindParameterName :: Statement -> Int -> IO (Maybe String)
bindParameterName (Statement stmt) idx = do
  mn <- c_sqlite3_bind_parameter_name stmt (toParamIndex idx) >>= maybeNullCString
  return (mn >>= return . T.unpack . T.decodeUtf8)

bindBlobError :: Statement -> Int -> BS.ByteString -> IO Error
bindBlobError (Statement statement) parameterIndex byteString = do
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\dataC -> do
                     error <- c_sqlite3_bind_blob statement
                                                  (toParamIndex parameterIndex)
                                                  dataC
                                                  (toCNumBytes size)
                                                  c_SQLITE_TRANSIENT
                     return $ decodeError error)
bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString = do
  error <- bindBlobError statement parameterIndex byteString
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind blob" error

bindDoubleError :: Statement -> Int -> Double -> IO Error
bindDoubleError (Statement statement) parameterIndex datum = do
  error <- c_sqlite3_bind_double statement
                                 (toParamIndex parameterIndex)
                                 datum
  return $ decodeError error
bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble statement parameterIndex datum = do
  error <- bindDoubleError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind double" error

bindIntError :: Statement -> Int -> Int -> IO Error
bindIntError (Statement statement) parameterIndex datum = do
  error <- c_sqlite3_bind_int64 statement
                                (toParamIndex parameterIndex)
                                (fromIntegral datum)
  return $ decodeError error

bindInt :: Statement -> Int -> Int -> IO ()
bindInt statement parameterIndex datum = do
  error <- bindIntError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int" error

bindInt64Error :: Statement -> Int -> Int64 -> IO Error
bindInt64Error (Statement statement) parameterIndex datum = do
  error <- c_sqlite3_bind_int64 statement
                                (toParamIndex parameterIndex)
                                datum
  return $ decodeError error
bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 statement parameterIndex datum = do
  error <- bindInt64Error statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int64" error

bindNullError :: Statement -> Int -> IO Error
bindNullError (Statement statement) parameterIndex = do
  error <- c_sqlite3_bind_null statement (toParamIndex parameterIndex)
  return $ decodeError error
bindNull :: Statement -> Int -> IO ()
bindNull statement parameterIndex = do
  error <- bindNullError statement parameterIndex
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind null" error

bindTextError :: Statement -> Int -> T.Text -> IO Error
bindTextError (Statement statement) parameterIndex text = do
  byteString <- return $ T.encodeUtf8 text
  size <- return $ BS.length byteString
  BS.useAsCString byteString
                  (\dataC -> do
                     error <- c_sqlite3_bind_text statement
                                                  (toParamIndex parameterIndex)
                                                  dataC
                                                  (toCNumBytes size)
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

columnType :: Statement -> Int -> IO ColumnType
columnType (Statement statement) columnIndex =
  decodeColumnType <$> c_sqlite3_column_type statement (toColumnIndex columnIndex)

columnBlob :: Statement -> Int -> IO BS.ByteString
columnBlob (Statement statement) columnIndex = do
  size <- c_sqlite3_column_bytes statement (toColumnIndex columnIndex)
  BSI.create (fromCNumBytes size) $ \resultPtr -> do
    dataPtr <- c_sqlite3_column_blob statement (toColumnIndex columnIndex)
    if dataPtr /= nullPtr
        then BSI.memcpy resultPtr dataPtr (fromCNumBytes size)
        else return ()

columnInt64 :: Statement -> Int -> IO Int64
columnInt64 (Statement statement) columnIndex = do
  c_sqlite3_column_int64 statement (toColumnIndex columnIndex)

columnDouble :: Statement -> Int -> IO Double
columnDouble (Statement statement) columnIndex = do
  c_sqlite3_column_double statement (toColumnIndex columnIndex)

columnText :: Statement -> Int -> IO T.Text
columnText (Statement statement) columnIndex = do
  text <- c_sqlite3_column_text statement (toColumnIndex columnIndex)
  byteString <- BS.packCString text
  return $ T.decodeUtf8 byteString

columnCount :: Statement -> IO Int
columnCount (Statement statement) =
  fromColumnCount <$> c_sqlite3_column_count statement

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
