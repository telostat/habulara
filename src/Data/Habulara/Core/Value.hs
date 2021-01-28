{-# LANGUAGE FlexibleContexts #-}

module Data.Habulara.Core.Value where

import           Control.Monad.Except        (MonadError(throwError))
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.Csv                    as Cassava
import           Data.Habulara.Core.Class    (HabularaError(..))
import qualified Data.Habulara.Core.NonEmpty as NEV
import           Data.Scientific             (Scientific)
import           Data.String                 (IsString(..))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Time                   (Day, LocalTime)
import           Text.Read                   (readMaybe)


-- | Habulara row record field value type.
data Value =
    VEmpty
  | VRaw      !(NEV.NonEmpty B.ByteString)
  | VText     !(NEV.NonEmpty T.Text)
  | VInt      !Integer
  | VDecimal  !Scientific
  | VBoolean  !Bool
  | VDate     !Day
  | VDateTime !LocalTime
  deriving (Eq, Ord, Show)


-- | 'IsString' instance for 'Value' type.
--
-- >>> "" :: Value
-- VEmpty
-- >>> "Hebele" :: Value
-- VRaw (MkNonEmpty {unpack = "Hebele"})
instance IsString Value where
  fromString = toValue . BC.pack


-- | @cassava@ 'Csv.FromField' instance for 'Value' values.
--
-- We are parsing the CSV column into either of:
--
-- 1. 'VEmpty' if the column value is an empty 'B.ByteString', or
-- 2. 'VRaw' otherwise.
--
-- We have chosen 'VRaw' for efficiency reasons. Extra check for 'VEmpty' is a
-- cost but sounds worth to bear: We will not operate on empty values during
-- operations.
--
-- See the underlying implementation in the 'Valuable' instance implementation
-- 'fromByteString' for 'Value'.
--
-- >>> Cassava.runParser $ Cassava.parseField "" :: Either String Value
-- Right VEmpty
-- >>> Cassava.runParser $ Cassava.parseField " " :: Either String Value
-- Right (VRaw (MkNonEmpty {unpack = " "}))
-- >>> Cassava.runParser $ Cassava.parseField " a " :: Either String Value
-- Right (VRaw (MkNonEmpty {unpack = " a "}))
-- >>> Cassava.runParser $ Cassava.parseField " 語 " :: Either String Value
-- Right (VRaw (MkNonEmpty {unpack = " \158 "}))
-- >>> Cassava.runParser $ Cassava.parseField " \t\r\n " :: Either String Value
-- Right (VRaw (MkNonEmpty {unpack = " \t\r\n "}))
instance Cassava.FromField Value where
  parseField = either (fail . show) pure . fromByteString


-- | @cassava@ 'Csv.ToField' instance for 'Value' values.
--
-- >>> Cassava.toField VEmpty
-- ""
-- >>> Cassava.toField $ VRaw "語"
-- "\158"
-- >>> Cassava.toField $ VInt 42
-- "42"
-- >>> Cassava.toField $ VText "Hello"
-- "Hello"
-- >>> Cassava.toField $ VDecimal (read "42")
-- "42.0"
-- >>> Cassava.toField $ VBoolean True
-- "True"
-- >>> Cassava.toField $ VBoolean False
-- "False"
-- >>> Cassava.toField $ VDate (read "2021-01-01")
-- "2021-01-01"
-- >>> Cassava.toField $ VDateTime (read "2021-01-01 23:59:59")
-- "2021-01-01 23:59:59"
instance Cassava.ToField Value where
  toField = toByteString


-- * Valuable Definition
--
-- $valuable


-- | A convenience class for the interplay between native and 'Value' types.
class (Eq a) => Valuable a where
  -- | Identity value.
  identity :: a

  -- | Checks if the value is identity.
  isIdentity :: a -> Bool
  isIdentity = (==) identity

  -- | Converts to the 'Value' value.
  toValue :: a -> Value

  -- | Attempts to convert from 'Value' value.
  fromValue :: MonadError HabularaError m => Value -> m a

  -- | Converts to a 'B.ByteString ('@cassava@ 'Csv.Field') value.
  toByteString :: a -> B.ByteString

  -- | Parses from a 'B.ByteString ('@cassava@ 'Csv.Field') value.
  fromByteString :: MonadError HabularaError m => B.ByteString -> m a

  -- | Provides a convenience function to throw a conversion error.
  raiseConversionError :: MonadError HabularaError m => String -> Value -> m a
  raiseConversionError t v = throwError . HabularaErrorValueConversion $ "Can not convert to " <> t <> ": " <> show v

  -- | Provides a convenience function to throw a read error.
  raiseReadError :: MonadError HabularaError m => String -> B.ByteString -> m a
  raiseReadError t x = throwError . HabularaErrorRead $ "Can not read " <> t <> " from: " <> BC.unpack x


-- | 'Valuable' instance for 'Value' type.
--
-- >>> toValue (VEmpty)
-- VEmpty
-- >>> fromValue (VEmpty) :: Either HabularaError Value
-- Right VEmpty
-- >>> toByteString (VEmpty)
-- ""
-- >>> fromByteString "" :: Either HabularaError Value
-- Right VEmpty
instance Valuable Value where
  identity = VEmpty

  toValue = id

  fromValue = pure

  toByteString VEmpty        = B.empty
  toByteString (VRaw i)      = NEV.unpack i
  toByteString (VText t)     = toByteString $ NEV.unpack t
  toByteString (VInt i)      = toByteString i
  toByteString (VDecimal d)  = toByteString d
  toByteString (VBoolean b)  = toByteString b
  toByteString (VDate d)     = toByteString d
  toByteString (VDateTime t) = toByteString t

  fromByteString = pure . maybe VEmpty VRaw . NEV.nonEmpty


-- | 'Valuable' instance for 'ByteString' type.
--
-- >>> toValue ("" :: B.ByteString)
-- VEmpty
-- >>> toValue (" " :: B.ByteString)
-- VRaw (MkNonEmpty {unpack = " "})
-- >>> toValue ("語" :: B.ByteString)
-- VRaw (MkNonEmpty {unpack = "\158"})
-- >>> fromValue VEmpty :: Either HabularaError B.ByteString
-- Right ""
-- >>> fromValue (VRaw " ") :: Either HabularaError B.ByteString
-- Right " "
-- >>> fromValue (VText " ") :: Either HabularaError B.ByteString
-- Left (HabularaErrorValueConversion "Can not convert to Integer: VText (MkNonEmpty {unpack = \" \"})")
-- >>> toByteString (VRaw " ")
-- " "
-- >>> fromByteString "" :: Either HabularaError B.ByteString
-- Right ""
-- >>> fromByteString " " :: Either HabularaError B.ByteString
-- Right " "
instance Valuable B.ByteString where
  identity = ""

  toValue = maybe VEmpty VRaw . NEV.nonEmpty

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue VEmpty   = pure ""
  fromValue (VRaw b) = pure $ NEV.unpack b
  fromValue v        = raiseConversionError "Integer" v

  toByteString = id

  fromByteString = pure


-- | 'Valuable' instance for 'T.Text' type.
--
-- >>> toValue ("" :: T.Text)
-- VEmpty
-- >>> toValue (" " :: T.Text)
-- VText (MkNonEmpty {unpack = " "})
-- >>> fromValue VEmpty :: Either HabularaError T.Text
-- Right ""
-- >>> fromValue (VText " ") :: Either HabularaError T.Text
-- Right " "
-- >>> toByteString (VText " ")
-- " "
-- >>> toByteString (VText "語")
-- "\232\170\158"
-- >>> fromByteString "" :: Either HabularaError T.Text
-- Right ""
-- >>> fromByteString " " :: Either HabularaError T.Text
-- Right " "
-- >>> fromByteString "\232\170\158" :: Either HabularaError T.Text
-- Right "\35486"
instance Valuable T.Text where
  identity = ""

  toValue = maybe VEmpty VText . NEV.nonEmpty

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue VEmpty    = pure ""
  fromValue (VText x) = pure $ NEV.unpack x
  fromValue v         = raiseConversionError "Text" v

  toByteString = TE.encodeUtf8

  -- TODO: Use decodeUtf8' to catch UnicodeException and propagate.
  fromByteString = pure . TE.decodeUtf8


-- | 'Valuable' instance for 'Integer' type.
--
-- >>> toValue (42 :: Integer)
-- VInt 42
-- >>> fromValue (VInt 42) :: Either HabularaError Integer
-- Right 42
-- >>> fromValue (VDecimal 42) :: Either HabularaError Integer
-- Left (HabularaErrorValueConversion "Can not convert to Integer: VDecimal 42.0")
-- >>> toByteString (VInt 42)
-- "42"
-- >>> fromByteString "42" :: Either HabularaError Integer
-- Right 42
-- >>> fromByteString "42a" :: Either HabularaError Integer
-- Left (HabularaErrorRead "Can not read Integer from: 42a")
instance Valuable Integer where
  identity = 0

  toValue = VInt

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VInt x) = pure x
  fromValue v        = raiseConversionError "Integer" v

  toByteString = BC.pack . show

  fromByteString b = case BC.readInteger b of
    Nothing     -> raiseReadError "Integer" b
    Just (x, r) -> if B.null r then pure x else raiseReadError "Integer" b


-- | 'Valuable' instance for 'Scientific' type.
--
-- >>> toValue (read "42" :: Scientific)
-- VDecimal 42.0
-- >>> fromValue VEmpty :: Either HabularaError Scientific
-- Left (HabularaErrorValueConversion "Can not convert to Scientific: VEmpty")
-- >>> fromValue (VDecimal $ read "42") :: Either HabularaError Scientific
-- Right 42.0
-- >>> toByteString (VDecimal $ read "42")
-- "42.0"
-- >>> fromByteString "42" :: Either HabularaError Scientific
-- Right 42.0
instance Valuable Scientific where
  identity = 0

  toValue = VDecimal

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDecimal x) = pure x
  fromValue v            = raiseConversionError "Scientific" v

  toByteString = BC.pack . show

  -- TODO: Find a more efficient method for this conversion.
  fromByteString b = case readMaybe $ BC.unpack b of
    Nothing -> raiseReadError "Scientific" b
    Just x  -> pure x


-- | 'Valuable' instance for 'Bool' type.
--
-- >>> toValue True
-- VBoolean True
-- >>> toValue False
-- VBoolean False
-- >>> fromValue VEmpty :: Either HabularaError Bool
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VEmpty")
-- >>> fromValue (VBoolean True) :: Either HabularaError Bool
-- Right True
-- >>> fromValue (VBoolean False) :: Either HabularaError Bool
-- Right False
-- >>> toByteString (VBoolean True)
-- "True"
-- >>> toByteString (VBoolean False)
-- "False"
-- >>> fromByteString "True" :: Either HabularaError Bool
-- Right True
-- >>> fromByteString "False" :: Either HabularaError Bool
-- Right False
instance Valuable Bool where
  identity = False

  toValue = VBoolean

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VBoolean True)  = pure True
  fromValue (VBoolean False) = pure False
  fromValue v                = raiseConversionError "Boolean" v

  toByteString True  = "True"
  toByteString False = "False"

  fromByteString "True"  = pure True
  fromByteString "False" = pure False
  fromByteString b       = raiseReadError "Boolean" b


-- | 'Valuable' instance for 'Day' type.
--
-- >>> toValue (read "2020-12-31" :: Day)
-- VDate 2020-12-31
-- >>> fromValue VEmpty :: Either HabularaError Day
-- Left (HabularaErrorValueConversion "Can not convert to Day: VEmpty")
-- >>> fromValue (VDate $ read "2020-12-31") :: Either HabularaError Day
-- Right 2020-12-31
-- >>> toByteString (VDate $ read "2020-12-31")
-- "2020-12-31"
-- >>> fromByteString "2020-12-31" :: Either HabularaError Day
-- Right 2020-12-31
instance Valuable Day where
  identity = read "0000-00-00"

  toValue = VDate

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDate x) = pure x
  fromValue v         = raiseConversionError "Day" v

  toByteString = BC.pack . show

  -- TODO: Find a more efficient method for this conversion.
  fromByteString b = case readMaybe $ BC.unpack b of
    Nothing -> raiseReadError "Date" b
    Just x  -> pure x


-- | 'Valuable' instance for 'LocalTime' type.
--
-- >>> toValue (read "2020-12-31 23:59:59" :: LocalTime)
-- VDateTime 2020-12-31 23:59:59
-- >>> fromValue VEmpty :: Either HabularaError LocalTime
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VEmpty")
-- >>> fromValue (VDateTime $ read "2020-12-31 23:59:59") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59
-- >>> toByteString (VDateTime $ read "2020-12-31 23:59:59")
-- "2020-12-31 23:59:59"
-- >>> fromByteString "2020-12-31 23:59:59" :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59
instance Valuable LocalTime where
  identity = read "0000-00-00 00:00:00"

  toValue = VDateTime

  -- TODO: Attempt to convert from other 'Value' values, too.
  fromValue (VDateTime x) = pure x
  fromValue v             = raiseConversionError "LocalTime" v

  toByteString = BC.pack . show

  -- TODO: Find a more efficient method for this conversion.
  fromByteString b = case readMaybe $ BC.unpack b of
    Nothing -> raiseReadError "Date" b
    Just x  -> pure x
