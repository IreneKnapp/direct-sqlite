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

import Database.SQLite3.Direct
    ( Database
    , Statement
    , Error(..)
    , StepResult(..)
    , ColumnType(..)
    , Utf8(..)
    )
import Database.SQLite3.Bindings.Types
    ( fromParamIndex, toParamIndex
    , toColumnIndex
    , fromColumnCount
    )

import qualified Database.SQLite3.Direct as Direct

import Prelude hiding (error)
import qualified Prelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative  ((<$>))
import Data.Int             (Int64)
import Data.String          (fromString)
import Data.Typeable


data SQLData = SQLInteger Int64
             | SQLFloat Double
             | SQLText T.Text
             | SQLBlob BS.ByteString
             | SQLNull
               deriving (Eq, Show, Typeable)

fromUtf8 :: Utf8 -> String
fromUtf8 (Utf8 bs) = (T.unpack . T.decodeUtf8) bs

toUtf8 :: String -> Utf8
toUtf8 = fromString

errmsg :: Database -> IO String
errmsg db = fromUtf8 <$> Direct.errmsg db

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

checkError :: Maybe Database -> String -> Either Error a -> IO a
checkError db fn = either (sqlError db fn) return

open :: String -> IO Database
open path = do
    Direct.open (toUtf8 path)
        >>= checkError Nothing ("open " ++ show path)

close :: Database -> IO ()
close db =
    Direct.close db >>= checkError (Just db) "close"

prepare :: Database -> String -> IO Statement
prepare db sql =
    Direct.prepare db (toUtf8 sql) >>=
        checkError (Just db) ("prepare " ++ (show sql))

step :: Statement -> IO StepResult
step statement =
    Direct.step statement >>= checkError Nothing "step"

reset :: Statement -> IO ()
reset statement =
    Direct.reset statement >>= checkError Nothing "reset"

finalize :: Statement -> IO ()
finalize statement =
    Direct.finalize statement >>= checkError Nothing "finalize"


-- | This returns the index of the largest (rightmost) parameter, which is not
-- necessarily the number of parameters.  If numbered parameters like @?5@ are
-- used, there may be gaps in the list.
bindParameterCount :: Statement -> IO Int
bindParameterCount stmt =
    fromParamIndex <$> Direct.bindParameterCount stmt

-- | Return the N-th SQL parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the parameter index starts at 1, not 0.
bindParameterName :: Statement -> Int -> IO (Maybe String)
bindParameterName stmt idx =
    fmap fromUtf8 <$>
    Direct.bindParameterName stmt (toParamIndex idx)

bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString =
    Direct.bindBlob statement (toParamIndex parameterIndex) byteString
        >>= checkError Nothing "bind blob"

bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble statement parameterIndex datum =
    Direct.bindDouble statement (toParamIndex parameterIndex) datum
        >>= checkError Nothing "bind double"

bindInt :: Statement -> Int -> Int -> IO ()
bindInt statement parameterIndex datum =
    Direct.bindInt64 statement
                     (toParamIndex parameterIndex)
                     (fromIntegral datum)
        >>= checkError Nothing "bind int"

bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 statement parameterIndex datum =
    Direct.bindInt64 statement
                     (toParamIndex parameterIndex)
                     datum
        >>= checkError Nothing "bind int64"

bindNull :: Statement -> Int -> IO ()
bindNull statement parameterIndex =
    Direct.bindNull statement (toParamIndex parameterIndex)
        >>= checkError Nothing "bind null"

bindText :: Statement -> Int -> T.Text -> IO ()
bindText statement parameterIndex text =
    Direct.bindText statement
                    (toParamIndex parameterIndex)
                    (Utf8 $ T.encodeUtf8 text)
        >>= checkError Nothing "bind text"

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
columnType statement columnIndex =
    Direct.columnType statement (toColumnIndex columnIndex)

columnBlob :: Statement -> Int -> IO BS.ByteString
columnBlob statement columnIndex =
    Direct.columnBlob statement (toColumnIndex columnIndex)

columnInt64 :: Statement -> Int -> IO Int64
columnInt64 statement columnIndex =
    Direct.columnInt64 statement (toColumnIndex columnIndex)

columnDouble :: Statement -> Int -> IO Double
columnDouble statement columnIndex =
    Direct.columnDouble statement (toColumnIndex columnIndex)

columnText :: Statement -> Int -> IO T.Text
columnText statement columnIndex = do
    Utf8 bs <- Direct.columnText statement (toColumnIndex columnIndex)
    return $! T.decodeUtf8 bs

columnCount :: Statement -> IO Int
columnCount statement =
    fromColumnCount <$> Direct.columnCount statement

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
