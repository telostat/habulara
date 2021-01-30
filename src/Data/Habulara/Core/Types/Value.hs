{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Habulara.Core.Types.Value where

import           Control.Monad                     ((>=>))
import           Control.Monad.Except              (MonadError(throwError))
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.Csv                          as Cassava
import           Data.Habulara.Core.Types.Class    (HabularaError(..), liftMaybe)
import qualified Data.Habulara.Core.Types.NonEmpty as NEV
import           Data.Scientific                   (Scientific)
import           Data.String                       (IsString(..))
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Data.Time
                 ( Day(..)
                 , LocalTime(..)
                 , NominalDiffTime
                 , TimeOfDay(..)
                 , addLocalTime
                 , diffLocalTime
                 , fromGregorian
                 , secondsToNominalDiffTime
                 )
import           Text.Read                         (readMaybe)


-- | Habulara row record field value type.
data Value =
    VEmpty
  | VText     !(NEV.NonEmpty T.Text)
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
-- VText (MkNonEmpty {unpack = "Hebele"})
instance IsString Value where
  fromString = toValue . T.pack


-- | @cassava@ 'Csv.FromField' instance for 'Value' values.
--
-- We are parsing the CSV column into either of:
--
-- 1. 'VEmpty' if the column value is an empty 'B.ByteString', or
-- 2. 'VText' otherwise.
--
-- See the underlying implementation in the 'Valuable' instance implementation
-- 'fromByteString' for 'Value'.
--
-- >>> Cassava.runParser $ Cassava.parseField "" :: Either String Value
-- Right VEmpty
-- >>> Cassava.runParser $ Cassava.parseField " " :: Either String Value
-- Right (VText (MkNonEmpty {unpack = " "}))
-- >>> Cassava.runParser $ Cassava.parseField " a " :: Either String Value
-- Right (VText (MkNonEmpty {unpack = " a "}))
-- >>> Cassava.runParser $ Cassava.parseField " 語 " :: Either String Value
-- *** Exception: Cannot decode byte '\x9e': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
-- >>> Cassava.runParser $ Cassava.parseField " \t\r\n " :: Either String Value
-- Right (VText (MkNonEmpty {unpack = " \t\r\n "}))
instance Cassava.FromField Value where
  parseField = either (fail . show) pure . fromByteString


-- | @cassava@ 'Csv.ToField' instance for 'Value' values.
--
-- >>> Cassava.toField VEmpty
-- ""
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

  -- | Converts to a 'T.Text' value.
  toText :: a -> T.Text
  toText = TE.decodeUtf8 . toByteString

  -- | Parses from a 'T.Text' value.
  fromText :: MonadError HabularaError m => T.Text -> m a
  fromText = fromByteString . TE.encodeUtf8

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
  toByteString (VText t)     = toByteString $ NEV.unpack t
  toByteString (VDecimal d)  = toByteString d
  toByteString (VBoolean b)  = toByteString b
  toByteString (VDate d)     = toByteString d
  toByteString (VDateTime t) = toByteString t

  fromByteString = pure . maybe VEmpty VText . NEV.nonEmpty . TE.decodeUtf8


-- | 'Valuable' instance for @'NonEmpty' 'T.Text'@ type.
--
-- >>> toValue (" " :: NEV.NonEmpty T.Text)
-- VText (MkNonEmpty {unpack = " "})
-- >>> fromValue VEmpty :: Either HabularaError (NEV.NonEmpty T.Text)
-- Left (HabularaErrorValueConversion "Can not create 'NonEmpty Text' with empty value")
-- >>> fromValue (" " :: Value) :: Either HabularaError (NEV.NonEmpty T.Text)
-- Right (MkNonEmpty {unpack = " "})
-- >>> toByteString (" " :: NEV.NonEmpty T.Text)
-- " "
-- >>> fromByteString "" :: Either HabularaError (NEV.NonEmpty T.Text)
-- Left (HabularaErrorValueConversion "Can not create 'NonEmpty Text' with empty value")
-- >>> fromByteString " " :: Either HabularaError (NEV.NonEmpty T.Text)
-- Right (MkNonEmpty {unpack = " "})
instance Valuable (NEV.NonEmpty T.Text) where
  identity = error "NonEmpty Text does not have identity value"

  toValue = VText

  fromValue VEmpty    = throwError $ HabularaErrorValueConversion "Can not create 'NonEmpty Text' with empty value"
  fromValue (VText t) = pure t
  fromValue v         = fromByteString . toByteString $ v

  toByteString = toByteString . NEV.unpack

  fromByteString = fromByteString >=> liftMaybe (HabularaErrorValueConversion "Can not create 'NonEmpty Text' with empty value") . NEV.nonEmpty


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

  fromValue VEmpty    = pure ""
  fromValue (VText t) = pure . NEV.unpack $ t
  fromValue v         = fromByteString . toByteString $ v

  toByteString = TE.encodeUtf8

  -- TODO: Use decodeUtf8' to catch UnicodeException and propagate.
  fromByteString = pure . TE.decodeUtf8


-- | 'Valuable' instance for 'Scientific' type.
--
-- >>> toValue (42 :: Scientific)
-- VDecimal 42.0
-- >>> fromValue VEmpty :: Either HabularaError Scientific
-- Right 0.0
-- >>> fromValue (VText "1") :: Either HabularaError Scientific
-- Right 1.0
-- >>> fromValue (VText "1a") :: Either HabularaError Scientific
-- Left (HabularaErrorRead "Can not read Scientific from: 1a")
-- >>> fromValue (VDecimal 42) :: Either HabularaError Scientific
-- Right 42.0
-- >>> fromValue (VBoolean False) :: Either HabularaError Scientific
-- Right 0.0
-- >>> fromValue (VBoolean True) :: Either HabularaError Scientific
-- Right 1.0
-- >>> fromValue (VDate $ read "2020-12-31") :: Either HabularaError Scientific
-- Right 59214.0
-- >>> fromValue (VDateTime $ read "2020-12-31 23:59:59.001") :: Either HabularaError Scientific
-- Right 1.609459199001e9
-- >>> toByteString (VDecimal 42)
-- "42.0"
-- >>> fromByteString "42" :: Either HabularaError Scientific
-- Right 42.0
-- >>> fromByteString "42a" :: Either HabularaError Scientific
-- Left (HabularaErrorRead "Can not read Scientific from: 42a")
instance Valuable Scientific where
  identity = 0

  toValue = VDecimal

  fromValue VEmpty           = pure identity
  fromValue (VDecimal x)     = pure x
  fromValue (VBoolean False) = pure 0
  fromValue (VBoolean True)  = pure 1
  fromValue (VDate x)        = pure . fromIntegral . toModifiedJulianDay $ x
  fromValue (VDateTime x)    = pure . realToFrac $ epoch x
  fromValue v                = fromByteString . toByteString $ v

  toByteString = BC.pack . show  -- TODO: Any faster way of doing this?

  fromByteString b = case readMaybe $ BC.unpack b of -- TODO: Any faster way of doing this?
    Nothing -> raiseReadError "Scientific" b
    Just x  -> pure x


-- | 'Valuable' instance for 'Bool' type.
--
-- >>> toValue True
-- VBoolean True
-- >>> toValue False
-- VBoolean False
-- >>> fromValue VEmpty :: Either HabularaError Bool
-- Right False
-- >>> fromValue (VText "0") :: Either HabularaError Bool
-- Left (HabularaErrorRead "Can not read Boolean from: 0")
-- >>> fromValue (VText "1") :: Either HabularaError Bool
-- Left (HabularaErrorRead "Can not read Boolean from: 1")
-- >>> fromValue (VText "True") :: Either HabularaError Bool
-- Right True
-- >>> fromValue (VText "False") :: Either HabularaError Bool
-- Right False
-- >>> fromValue (VDecimal 0) :: Either HabularaError Bool
-- Right False
-- >>> fromValue (VDecimal 1) :: Either HabularaError Bool
-- Right True
-- >>> fromValue (VDecimal 42) :: Either HabularaError Bool
-- Right True
-- >>> fromValue (VBoolean False) :: Either HabularaError Bool
-- Right False
-- >>> fromValue (VBoolean True) :: Either HabularaError Bool
-- Right True
-- >>> fromValue (VDate $ read "2020-12-31") :: Either HabularaError Bool
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VDate 2020-12-31")
-- >>> fromValue (VDateTime $ read "2020-12-31 23:59:59.001") :: Either HabularaError Bool
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VDateTime 2020-12-31 23:59:59.001")
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

  fromValue VEmpty          = pure identity
  fromValue (VDecimal x)    = pure $ x /= 0
  fromValue (VBoolean x)    = pure x
  fromValue x@(VDate _)     = raiseConversionError "Boolean" x
  fromValue x@(VDateTime _) = raiseConversionError "Boolean" x
  fromValue v               = fromByteString . toByteString $ v

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
-- Right 1858-11-17
-- >>> fromValue (VText " ") :: Either HabularaError Day
-- Left (HabularaErrorRead "Can not read Date from:  ")
-- >>> fromValue (VText "2020-12-31") :: Either HabularaError Day
-- Right 2020-12-31
-- >>> fromValue (VDecimal 0) :: Either HabularaError Day
-- Right 1858-11-17
-- >>> fromValue (VDecimal 1) :: Either HabularaError Day
-- Right 1858-11-18
-- >>> fromValue (VDecimal 42) :: Either HabularaError Day
-- Right 1858-12-29
-- >>> fromValue (VBoolean False) :: Either HabularaError Day
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBoolean False")
-- >>> fromValue (VBoolean True) :: Either HabularaError Day
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBoolean True")
-- >>> fromValue (VDate $ read "2020-12-31") :: Either HabularaError Day
-- Right 2020-12-31
-- >>> fromValue (VDateTime $ read "2020-12-31 23:59:59.001") :: Either HabularaError Day
-- Right 2020-12-31
-- >>> toByteString (VDate $ read "2020-12-31")
-- "2020-12-31"
-- >>> fromByteString "2020-12-31" :: Either HabularaError Day
-- Right 2020-12-31
instance Valuable Day where
  identity = ModifiedJulianDay 0

  toValue = VDate

  fromValue VEmpty         = pure identity
  fromValue (VDecimal x)   = pure . ModifiedJulianDay . floor $ x
  fromValue x@(VBoolean _) = raiseConversionError "Date" x
  fromValue (VDate x)      = pure x
  fromValue (VDateTime x)  = pure . localDay $ x
  fromValue v              = fromByteString . toByteString $ v

  toByteString = BC.pack . show  -- TODO: Any faster way of doing this?

  fromByteString b = case readMaybe $ BC.unpack b of  -- TODO: Any faster way of doing this?
    Nothing -> raiseReadError "Date" b
    Just x  -> pure x


-- | 'Valuable' instance for 'LocalTime' type.
--
-- >>> toValue (read "2020-12-31 23:59:59" :: LocalTime)
-- VDateTime 2020-12-31 23:59:59
-- >>> fromValue VEmpty :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:00
-- >>> fromValue (VText "2020-12-31 23:59:59") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59
-- >>> fromValue (VText "2020-12-31 23:59:59.000001") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59.000001
-- >>> fromValue (VText "2020-12-31 23:59:59.000000001") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59.000000001
-- >>> fromValue (VText "2020-12-31 23:59:59.000000000001") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59.000000000001
-- >>> fromValue (VText "2020-12-31 23:59:59.0000000000009") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59
-- >>> fromValue (VDecimal 0) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:00
-- >>> fromValue (VDecimal 1) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:01
-- >>> fromValue (VDecimal 42) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42
-- >>> fromValue (VDecimal 42.001) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42.001
-- >>> fromValue (VDecimal 42.000001) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42.000001
-- >>> fromValue (VDecimal 42.000000001) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42.000000001
-- >>> fromValue (VDecimal 42.000000000001) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42.000000000001
-- >>> fromValue (VDecimal 42.0000000000009) :: Either HabularaError LocalTime
-- Right 1970-01-01 00:00:42
-- >>> fromValue (VBoolean False) :: Either HabularaError LocalTime
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBoolean False")
-- >>> fromValue (VBoolean True) :: Either HabularaError LocalTime
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBoolean True")
-- >>> fromValue (VDate $ read "2020-12-31") :: Either HabularaError LocalTime
-- Right 2020-12-31 00:00:00
-- >>> fromValue (VDateTime $ read "2020-12-31 23:59:59.001") :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59.001
-- >>> toByteString (VDateTime $ read "2020-12-31 23:59:59")
-- "2020-12-31 23:59:59"
-- >>> fromByteString "2020-12-31 23:59:59" :: Either HabularaError LocalTime
-- Right 2020-12-31 23:59:59
instance Valuable LocalTime where
  identity = epochStart

  toValue = VDateTime

  fromValue VEmpty         = pure identity
  fromValue (VDecimal x)   = pure . flip addLocalTime epochStart . secondsToNominalDiffTime . fromRational . toRational $ x
  fromValue x@(VBoolean _) = raiseConversionError "LocalTime" x
  fromValue (VDate x)      = pure . flip LocalTime (TimeOfDay 0 0 0) $ x
  fromValue (VDateTime x)  = pure x
  fromValue v              = fromByteString . toByteString $ v

  toByteString = BC.pack . show  -- TODO: Any faster way of doing this?

  fromByteString b = case readMaybe $ BC.unpack b of  -- TODO: Any faster way of doing this?
    Nothing -> raiseReadError "LocalTime" b
    Just x  -> pure x


-- * Auxiliaries
--
-- $auxiliaries


-- | Local time corresponding to UNIX epoch start time.
--
-- >>> epochStart
-- 1970-01-01 00:00:00
epochStart :: LocalTime
epochStart = LocalTime (fromGregorian 1970 1 1) (TimeOfDay 0 0 0)


-- | Computes epoch time as 'NominalDiffTime'.
--
-- >>> epoch epochStart
-- 0s
-- >>> epoch (read "2020-12-31 23:59:59")
-- 1609459199s
-- >>> epoch (read "2020-12-31 23:59:59.999999999999")
-- 1609459199.999999999999s
epoch :: LocalTime -> NominalDiffTime
epoch = flip diffLocalTime epochStart
