{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Core.Types where

import           Control.Monad.Except     (MonadError(throwError))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.Csv                 as Csv
import           Data.Habulara.Core.Class (HabularaError(..))
import qualified Data.HashMap.Strict      as HM
import           Data.Scientific          (Scientific)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Time                (Day, LocalTime)


-- | Habulara row record type.
--
-- A row record is a 'HM.HashMap' of keys of type 'Label' and values of type
-- 'Value'.
type Record = HM.HashMap Label Value


-- | Habulara row record field name type (key type).
type Label = T.Text


-- | Habulara row record field value type.
data Value =
    VEmpty
  | VInt      !Integer
  | VText     !T.Text
  | VDecimal  !Scientific
  | VBoolean  !Bool
  | VDate     !Day
  | VDateTime !LocalTime
  deriving (Show)


-- | @cassava@ 'Csv.FromField' instance for 'Value' values.
--
-- We are parsing the CSV column into either of:
--
-- 1. 'VEmpty' if the column value is an empty text, or
-- 2. 'VText' otherwise.
--
-- >>> import Data.Csv (parseField, runParser)
-- >>> runParser $ parseField "" :: Either String Value
-- Right VEmpty
-- >>> runParser $ parseField " " :: Either String Value
-- Right (VText " ")
-- >>> runParser $ parseField " a " :: Either String Value
-- Right (VText " a ")
-- >>> runParser $ parseField " \t\r\n " :: Either String Value
-- Right (VText " \t\r\n ")
instance Csv.FromField Value where
  -- TODO: Use decodeUtf8' to catch UnicodeException and propagate.
  parseField x
    | B.null x  = pure VEmpty
    | otherwise = pure . VText . TE.decodeUtf8 $ x


-- | @cassava@ 'Csv.ToField' instance for 'Value' values.
--
-- >>> import qualified Data.Csv
-- >>> Data.Csv.toField VEmpty
-- ""
-- >>> Data.Csv.toField $ VInt 42
-- "42"
-- >>> Data.Csv.toField $ VText "Hello"
-- "Hello"
-- >>> Data.Csv.toField $ VDecimal (read "42")
-- "42.0"
-- >>> Data.Csv.toField $ VBoolean True
-- "True"
-- >>> Data.Csv.toField $ VBoolean False
-- "False"
-- >>> Data.Csv.toField $ VDate (read "2021-01-01")
-- "2021-01-01"
-- >>> toField $ VDateTime (read "2021-01-01 23:59:59")
-- "2021-01-01 23:59:59"
instance Csv.ToField Value where
  toField = toField


-- | A convenience class for the interplay between native and 'Value' types.
class Valuable a where

  -- | Converts to the 'Value' value.
  toValue :: a -> Value

  -- | Attempts to convert from 'Value' value.
  fromValue :: MonadError HabularaError m => Value -> m a

  -- | Provides the @cassava@ 'Csv.Field' representation.
  toField :: a -> B.ByteString

  -- | Provides a convenience function to throw a conversion error.
  raiseConversionError :: MonadError HabularaError m => String -> Value -> m a
  raiseConversionError t v = throwError . HabularaErrorValueConversion $ "Can not convert to " <> t <> ": " <> show v


-- | 'Valuable' instance for 'Value' type.
instance Valuable Value where
  toValue = id

  fromValue = pure

  toField VEmpty        = B.empty
  toField (VInt i)      = toField i
  toField (VText t)     = toField t
  toField (VDecimal d)  = toField d
  toField (VBoolean b)  = toField  b
  toField (VDate d)     = toField d
  toField (VDateTime t) = toField t


-- | 'Valuable' instance for 'Integer' type.
instance Valuable Integer where
  toValue = VInt

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VInt x) = pure x
  fromValue v        = raiseConversionError "Integer" v

  toField = BC.pack . show


-- | 'Valuable' instance for 'T.Text' type.
instance Valuable T.Text where
  toValue = VText

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VText x) = pure x
  fromValue v         = raiseConversionError "Text" v

  toField = TE.encodeUtf8


-- | 'Valuable' instance for 'Scientific' type.
instance Valuable Scientific where
  toValue = VDecimal

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDecimal x) = pure x
  fromValue v            = raiseConversionError "Scientific" v

  toField = BC.pack . show


-- | 'Valuable' instance for 'Bool' type.
instance Valuable Bool where
  toValue = VBoolean

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VBoolean True)  = pure True
  fromValue (VBoolean False) = pure False
  fromValue v                = raiseConversionError "Boolean" v

  toField True  = "True"
  toField False = "False"


-- | 'Valuable' instance for 'Day' type.
instance Valuable Day where
  toValue = VDate

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDate x) = pure x
  fromValue v         = raiseConversionError "Day" v

  toField = BC.pack . show


-- | 'Valuable' instance for 'LocalTime' type.
instance Valuable LocalTime where
  toValue = VDateTime

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDateTime x) = pure x
  fromValue v             = raiseConversionError "LocalTime" v

  toField = BC.pack . show
