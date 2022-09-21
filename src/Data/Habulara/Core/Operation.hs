-- | This module provides primitives and pre-defined convenience functions used
-- to build Habulara 'Record's from raw 'Record's.
--
-- As a quick start:
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaIO)
-- >>> runHabularaIO (1, HM.fromList [("a", "1")]) (HM.empty :: Record) (select "a" >>= asEmpty)
-- Right (VEmpty,fromList [])
-- >>> runHabularaIO (1, HM.fromList [("a", "1")] :: Record) (HM.fromList [("b", "1")]) (peek "b" >>= asEmpty)
-- Right (VEmpty,fromList [("b",VText (MkNonEmpty {unpack = "1"}))])

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Habulara.Core.Operation where

import           Control.Applicative               (Alternative((<|>)))
import           Control.Monad.Except              (MonadError(..))
import           Control.Monad.Reader              (MonadReader)
import           Control.Monad.State               (MonadState)
import           Data.Habulara.Core.Mapping        (MapperEnvir, MapperState, askLabel, getLabel)
import           Data.Habulara.Core.Types.Class    (HabularaError(..), HabularaT, liftMaybe, runHabularaIO)
import qualified Data.Habulara.Core.Types.NonEmpty as NEV
import           Data.Habulara.Core.Types.Value    (Valuable(..), Value(..))
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe)
import           Data.Scientific                   (Scientific, toRealFloat)
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import           Data.Time                         (Day, LocalTime, defaultTimeLocale, parseTimeM)
import qualified Data.Time
import           Prelude                           hiding (and, drop, lookup, not, or, subtract, take)
import qualified Prelude


-- $setup
-- >>> import Control.Monad ((>=>))
-- >>> import Data.Habulara.Core.Types.Class  (runHabularaIO)
-- >>> import Data.Habulara.Core.Types.Record (Record)
-- >>> import qualified Data.HashMap.Strict   as HM
-- >>> let record = HM.fromList [("a", VEmpty), ("b", true), ("c", VNumber 1), ("d", "test")] :: Record
-- >>> let envir = (1, record)
-- >>> let state = HM.fromList [("a", VText "#N/A")] :: Record


-- * Field Accessors
--
-- These operations are essential in that they allow accessing raw record or
-- buffer record field values.
--
-- $operationsFieldAccessors


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- If the label does not exist in the record, 'VEmpty' is returned.
--
-- >>> testop envir state (lookup "a")
-- Right VEmpty
-- >>> testop envir state (lookup "b")
-- Right (VBool True)
-- >>> testop envir state (lookup "hebelehubele")
-- Right VEmpty
lookup :: (MonadReader MapperEnvir m, MonadError HabularaError m, Alternative m) => Value -> m Value
lookup s = select s <|> pure VEmpty


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- Similar to 'lookup' but if the label does not exist in the record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> testop envir state (select "a")
-- Right VEmpty
-- >>> testop envir state (select "b")
-- Right (VBool True)
-- >>> testop envir state (select "hebelehubele")
-- Left (HabularaErrorOperation "Can not find record field with label: hebelehubele")
select :: (MonadReader MapperEnvir m, MonadError HabularaError m) => Value -> m Value
select = withText (\s -> askLabel s >>= liftMaybe (HabularaErrorOperation $ "Can not find record field with label: " <> s))


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- state buffer record (record that is currently built up).
--
-- If the label does not exist in the state buffer record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> testop envir state (peek "a")
-- Right (VText (MkNonEmpty {unpack = "#N/A"}))
-- >>> testop envir state (peek "hebelehubele")
-- Left (HabularaErrorOperation "Can not find buffer record field with label: hebelehubele")
peek :: (MonadState MapperState m, MonadError HabularaError m) => Value -> m Value
peek = withText (\s -> getLabel s >>= liftMaybe (HabularaErrorOperation $ "Can not find buffer record field with label: " <> s))


-- * Value Constructors
--
-- These operations are primitives for 'Value' construction.
--
-- $operationsValueConstructors


-- >>> empty
-- VEmpty
empty :: Value
empty = VEmpty


-- >>> bool True
-- VBool True
-- >>> bool False
-- VBool False
bool :: Bool -> Value
bool = toValue


-- >>> true
-- VBool True
true :: Value
true = bool True


-- >>> false
-- VBool False
false :: Value
false = bool False


-- >>> date (read "2020-12-31")
-- VDate 2020-12-31
date :: Day -> Value
date = toValue


-- >>> time (read "2020-12-31 23:59:59")
-- VTime 2020-12-31 23:59:59
time :: LocalTime -> Value
time = toValue


-- >>> number 42
-- VNumber 42.0
number :: Scientific -> Value
number = toValue


-- >>> text ""
-- VEmpty
-- >>> text " "
-- VText (MkNonEmpty {unpack = " "})
text :: T.Text -> Value
text = toValue


-- * Value Converters
--
-- These operations are primitives for 'Value' conversions.
--
-- $operationsValueConverters


-- | Converts the value to a 'VEmpty' value.
--
-- >>> testopV $ asEmpty VEmpty
-- Right VEmpty
-- >>> testopV $ asEmpty (text " ")
-- Right VEmpty
asEmpty :: Monad m => Value -> m Value
asEmpty = pure . const VEmpty


-- | Attempts to convert the given 'Value' to a 'VBool' value.
--
-- >>> testopV $ asBool VEmpty
-- Right (VBool False)
-- >>> testopV $ asBool (VText "語")
-- Left (HabularaErrorRead "Can not read Boolean from: \232\170\158")
-- >>> testopV $ asBool (VNumber 42.0)
-- Right (VBool True)
-- >>> testopV $ asBool (VBool True)
-- Right (VBool True)
-- >>> testopV $ asBool (VBool False)
-- Right (VBool False)
-- >>> testopV $ asBool (VDate $ read "2020-12-31")
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VDate 2020-12-31")
-- >>> testopV $ asBool (VTime $ read "2020-12-31 23:59:59")
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VTime 2020-12-31 23:59:59")
asBool :: MonadError HabularaError m => Value -> m Value
asBool x = bool <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VDate' value.
--
-- >>> testopV $ asDate VEmpty
-- Right (VDate 1858-11-17)
-- >>> testopV $ asDate (VText "語")
-- Left (HabularaErrorRead "Can not read Date from: \232\170\158")
-- >>> testopV $ asDate (VNumber 42.0)
-- Right (VDate 1858-12-29)
-- >>> testopV $ asDate (VBool True)
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBool True")
-- >>> testopV $ asDate (VBool False)
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBool False")
-- >>> testopV $ asDate (VDate $ read "2020-12-31")
-- Right (VDate 2020-12-31)
-- >>> testopV $ asDate (VTime $ read "2020-12-31 23:59:59")
-- Right (VDate 2020-12-31)
asDate :: MonadError HabularaError m => Value -> m Value
asDate x = date <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VTime' value.
--
-- >>> testopV $ asTime VEmpty
-- Right (VTime 1970-01-01 00:00:00)
-- >>> testopV $ asTime (VText "語")
-- Left (HabularaErrorRead "Can not read LocalTime from: \232\170\158")
-- >>> testopV $ asTime (VNumber 42.0)
-- Right (VTime 1970-01-01 00:00:42)
-- >>> testopV $ asTime (VBool True)
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBool True")
-- >>> testopV $ asTime (VBool False)
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBool False")
-- >>> testopV $ asTime (VDate $ read "2020-12-31")
-- Right (VTime 2020-12-31 00:00:00)
-- >>> testopV $ asTime (VTime $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59)
asTime :: MonadError HabularaError m => Value -> m Value
asTime x = time <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VNumber' value.
--
-- >>> testopV $ asNumber VEmpty
-- Right (VNumber 0.0)
-- >>> testopV $ asNumber (VText "語")
-- Left (HabularaErrorRead "Can not read Scientific from: \232\170\158")
-- >>> testopV $ asNumber (VNumber 42.0)
-- Right (VNumber 42.0)
-- >>> testopV $ asNumber (VBool True)
-- Right (VNumber 1.0)
-- >>> testopV $ asNumber (VBool False)
-- Right (VNumber 0.0)
-- >>> testopV $ asNumber (VDate $ read "2020-12-31")
-- Right (VNumber 59214.0)
-- >>> testopV $ asNumber (VTime $ read "2020-12-31 23:59:59")
-- Right (VNumber 1.609459199e9)
asNumber :: MonadError HabularaError m => Value -> m Value
asNumber x = number <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VText' value.
--
-- >>> testopV $ asText VEmpty
-- Right VEmpty
-- >>> testopV $ asText (VText "語")
-- Right (VText (MkNonEmpty {unpack = "\35486"}))
-- >>> testopV $ asText (VNumber 42.0)
-- Right (VText (MkNonEmpty {unpack = "42.0"}))
-- >>> testopV $ asText (VBool True)
-- Right (VText (MkNonEmpty {unpack = "True"}))
-- >>> testopV $ asText (VBool False)
-- Right (VText (MkNonEmpty {unpack = "False"}))
-- >>> testopV $ asText (VDate $ read "2020-12-31")
-- Right (VText (MkNonEmpty {unpack = "2020-12-31"}))
-- >>> testopV $ asText (VTime $ read "2020-12-31 23:59:59")
-- Right (VText (MkNonEmpty {unpack = "2020-12-31 23:59:59"}))
asText :: MonadError HabularaError m => Value -> m Value
asText x = text <$> fromValue x


-- * Value Subtype Guarded Operators
--
-- $ operatorsValueSubtypeGuards


-- | Runs the action if the value is a 'VEmpty', raises error otherwise.
--
-- >>> testopV $ withEmpty (pure true) VEmpty
-- Right (VBool True)
-- >>> testopV $ withEmpty (pure true) true
-- Left (HabularaErrorOperation "Expecting 'VEmpty', recieved: VBool True")
withEmpty :: MonadError HabularaError m => m a -> Value -> m a
withEmpty a VEmpty = a
withEmpty _ v      = raiseOperationTypeGuard "VEmpty" v


-- | Runs the action if the value is a 'VBool', raises error otherwise.
--
-- >>> testopV $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) true
-- Right (VText (MkNonEmpty {unpack = "Yes"}))
-- >>> testopV $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) false
-- Right (VText (MkNonEmpty {unpack = "No"}))
-- >>> testopV $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) VEmpty
-- Left (HabularaErrorOperation "Expecting 'VBool', recieved: VEmpty")
withBool :: MonadError HabularaError m => (Bool -> m a) -> Value -> m a
withBool f (VBool x) = f x
withBool _ v         = raiseOperationTypeGuard "VBool" v


-- | Runs the action if the value is a 'VDate', raises error otherwise.
--
-- >>> testopV $ withDate (\x -> pure $ "Today is " <> show x) (VDate $ read "2020-12-31")
-- Right "Today is 2020-12-31"
-- >>> testopV $ withDate (\x -> pure $ "Today is " <> show x) true
-- Left (HabularaErrorOperation "Expecting 'VDate', recieved: VBool True")
withDate :: MonadError HabularaError m => (Day -> m a) -> Value -> m a
withDate f (VDate x) = f x
withDate _ v         = raiseOperationTypeGuard "VDate" v


-- | Runs the action if the value is a 'VTime', raises error otherwise.
--
-- >>> testopV $ withTime (\x -> pure $ "Now is " <> show x) (VTime $ read "2020-12-31 23:59:59")
-- Right "Now is 2020-12-31 23:59:59"
-- >>> testopV $ withTime (\x -> pure $ "Now is " <> show x) true
-- Left (HabularaErrorOperation "Expecting 'VTime', recieved: VBool True")
withTime :: MonadError HabularaError m => (LocalTime -> m a) -> Value -> m a
withTime f (VTime x) = f x
withTime _ v         = raiseOperationTypeGuard "VTime" v


-- | Runs the action if the value is a 'VNumber', raises error otherwise.
--
-- >>> testopV $ withNumber (pure . (1 +)) (VNumber 42)
-- Right 43.0
-- >>> testopV $ withNumber (pure . (1 +)) true
-- Left (HabularaErrorOperation "Expecting 'VNumber', recieved: VBool True")
withNumber :: MonadError HabularaError m => (Scientific -> m a) -> Value -> m a
withNumber f (VNumber x) = f x
withNumber _ v           = raiseOperationTypeGuard "VNumber" v


-- | Runs the action if the value is a 'VText', raises error otherwise.
--
-- >>> testopV $ withText (pure . ("Hello " <>)) "World"
-- Right "Hello World"
-- >>> testopV $ withText (pure . ("Hello " <>)) true
-- Left (HabularaErrorOperation "Expecting 'VText', recieved: VBool True")
withText :: MonadError HabularaError m => (T.Text -> m a) -> Value -> m a
withText f (VText x) = f . NEV.unpack $ x
withText _ v         = raiseOperationTypeGuard "VText" v


-- * Boolean Operators
--
-- $operatorsBoolean


-- | Boolean @not@ operation.
--
-- >>> testopV $ not false
-- Right (VBool True)
-- >>> testopV $ not true
-- Right (VBool False)
not :: MonadError HabularaError m => Value -> m Value
not = unaryOperation withBool (liftBool . Prelude.not)


-- | Boolean @and@ operation.
--
-- >>> testopV $ and false false
-- Right (VBool False)
-- >>> testopV $ and false true
-- Right (VBool False)
-- >>> testopV $ and true false
-- Right (VBool False)
-- >>> testopV $ and true true
-- Right (VBool True)
and :: MonadError HabularaError m => Value -> Value -> m Value
and = binaryOperation withBool (\x y -> liftBool $ (&&) x y)


-- | Boolean @or@ operation.
--
-- >>> testopV $ or false false
-- Right (VBool False)
-- >>> testopV $ or false true
-- Right (VBool True)
-- >>> testopV $ or true false
-- Right (VBool True)
-- >>> testopV $ or true true
-- Right (VBool True)
or :: MonadError HabularaError m => Value -> Value -> m Value
or = binaryOperation withBool (\x y -> liftBool $ (||) x y)


-- | Boolean @xor@ operation.
--
-- >>> testopV $ xor false false
-- Right (VBool False)
-- >>> testopV $ xor false true
-- Right (VBool True)
-- >>> testopV $ xor true false
-- Right (VBool True)
-- >>> testopV $ xor true true
-- Right (VBool False)
xor :: MonadError HabularaError m => Value -> Value -> m Value
xor = binaryOperation withBool (\x y -> liftBool $ (/=) x y)


-- * Date Operators
--
-- $operatorsDate


-- | Parses a date.
--
-- >>> testopV $ parseDate "%Y%m%d" "20201231"
-- Right (VDate 2020-12-31)
-- >>> testopV $ parseDate "%Y%m%d" "2020-12-31"
-- Left (HabularaErrorOperation "Can not parse date. Format: %Y%m%d. Value: 2020-12-31")
parseDate :: MonadError HabularaError m => Value -> Value -> m Value
parseDate = binaryOperation
  withText
  (\f v -> maybe
    (raiseOperationError $ "Can not parse date. Format: " <> f <> ". Value: " <> v)
    (pure . VDate)
    (parseTimeM True defaultTimeLocale (T.unpack f) (T.unpack v)))


-- | Adds days to a date.
--
-- >>> testopV $ addDays (number 0) (date $ read "2020-12-31")
-- Right (VDate 2020-12-31)
-- >>> testopV $ addDays (number 1) (date $ read "2020-12-31")
-- Right (VDate 2021-01-01)
-- >>> testopV $ addDays (number (-1)) (date $ read "2020-12-31")
-- Right (VDate 2020-12-30)
addDays :: MonadError HabularaError m => Value -> Value -> m Value
addDays x y = withNumber (\n -> withDate (liftDate . Data.Time.addDays (floor n)) y) x


-- * Time Operators
--
-- $operatorsTime


-- | Parses a time.
--
-- >>> testopV $ parseTime "%Y%m%d%H%M%S" "20201231235959"
-- Right (VTime 2020-12-31 23:59:59)
-- >>> testopV $ parseTime "%Y%m%d%H%M%S" "2020-12-31 23:59:59"
-- Left (HabularaErrorOperation "Can not parse time. Format: %Y%m%d%H%M%S. Value: 2020-12-31 23:59:59")
parseTime :: MonadError HabularaError m => Value -> Value -> m Value
parseTime = binaryOperation
  withText
  (\f v -> maybe
    (raiseOperationError $ "Can not parse time. Format: " <> f <> ". Value: " <> v)
    (pure . VTime)
    (parseTimeM True defaultTimeLocale (T.unpack f) (T.unpack v)))


-- | Adds seconds to a date/time.
--
-- >>> testopV $ addSeconds (number 0) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59)
-- >>> testopV $ addSeconds (number 0.9) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59.9)
-- >>> testopV $ addSeconds (number 1) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2021-01-01 00:00:00)
-- >>> testopV $ addSeconds (number (-1)) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:58)
addSeconds :: MonadError HabularaError m => Value -> Value -> m Value
addSeconds x y = withNumber (\n -> withTime (liftTime . Data.Time.addLocalTime (fromRational $ toRational n)) y) x


-- * Numeric Operators
--
-- $operatorsNumeric


-- | Addition operation.
--
-- >>> testopV $ add (VNumber 41) (VNumber 1)
-- Right (VNumber 42.0)
add :: MonadError HabularaError m => Value -> Value -> m Value
add = binaryOperation withNumber (\x y -> liftNumber $ (+) x y)


-- | Subtraction operation.
--
-- >>> testopV $ subtract (VNumber 43) (VNumber 1)
-- Right (VNumber 42.0)
-- >>> testopV $ flip subtract (VNumber 43) (VNumber 1)
-- Right (VNumber -42.0)
subtract :: MonadError HabularaError m => Value -> Value -> m Value
subtract = binaryOperation withNumber (\x y -> liftNumber $ (-) x y)


-- | Multiplication operation.
--
-- >>> testopV $ multiply (VNumber 6) (VNumber 7)
-- Right (VNumber 42.0)
multiply :: MonadError HabularaError m => Value -> Value -> m Value
multiply = binaryOperation withNumber (\x y -> liftNumber $ (*) x y)


-- | Division operation.
--
-- >>> testopV $ divide (VNumber 84) (VNumber 2)
-- Right (VNumber 42.0)
-- >>> testopV $ flip divide (VNumber 84) (VNumber 2)
-- Right (VNumber 2.3809523809523808e-2)
divide :: MonadError HabularaError m => Value -> Value -> m Value
divide = binaryOperation withNumber (\x y -> liftNumber $ floatingDiv x y)
  where
    floatingDiv :: Scientific -> Scientific -> Scientific  -- TODO: Find a better solution
    floatingDiv x y = read . show $ (toRealFloat x :: Double) / (toRealFloat y :: Double)


-- | Converts a number to percentage points.
--
-- >>> testopV $ percentage (VNumber 0)
-- Right (VNumber 0.0)
-- >>> testopV $ percentage (VNumber 0.5)
-- Right (VNumber 50.0)
-- >>> testopV $ percentage (VNumber 1)
-- Right (VNumber 100.0)
-- >>> testopV $ percentage true
-- Left (HabularaErrorOperation "Expecting 'VNumber', recieved: VBool True")
percentage :: MonadError HabularaError m => Value -> m Value
percentage = unaryOperation withNumber (liftNumber . (*) 100)


-- * Textual Operators
--
-- $operatorsTextual


-- | Strips whitespace from the beginning and end of a textual value.
--
-- >>> testopV $ trim " "
-- Right VEmpty
-- >>> testopV $ trim " a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trim "a "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trim " a "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trim " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trim " a b "
-- Right (VText (MkNonEmpty {unpack = "a b"}))
-- >>> testopV $ trim " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b"}))
-- >>> testopV $ trim "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b"}))
trim :: MonadError HabularaError m => Value -> m Value
trim = unaryOperation withText (liftText . T.strip)


-- | Strips whitespace from the beginning of a textual value.
--
-- >>> testopV $ trimStart " "
-- Right VEmpty
-- >>> testopV $ trimStart " a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trimStart "a "
-- Right (VText (MkNonEmpty {unpack = "a "}))
-- >>> testopV $ trimStart " a "
-- Right (VText (MkNonEmpty {unpack = "a "}))
-- >>> testopV $ trimStart " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n "}))
-- >>> testopV $ trimStart " a b "
-- Right (VText (MkNonEmpty {unpack = "a b "}))
-- >>> testopV $ trimStart " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b "}))
-- >>> testopV $ trimStart "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b \t\r\n "}))
trimStart :: MonadError HabularaError m => Value -> m Value
trimStart = unaryOperation withText (liftText . T.stripStart)


-- | Strips whitespace from the end of a textual value.
--
-- >>> testopV $ trimEnd " "
-- Right VEmpty
-- >>> testopV $ trimEnd " a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ trimEnd "a "
-- Right (VText (MkNonEmpty {unpack = "a "}))
-- >>> testopV $ trimEnd " a "
-- Right (VText (MkNonEmpty {unpack = "a "}))
-- >>> testopV $ trimEnd " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n "}))
-- >>> testopV $ trimEnd " a b "
-- Right (VText (MkNonEmpty {unpack = "a b "}))
-- >>> testopV $ trimEnd " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b "}))
-- >>> testopV $ trimEnd "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b \t\r\n "}))
trimEnd :: MonadError HabularaError m => Value -> m Value
trimEnd = unaryOperation withText (liftText . T.stripStart)


-- | Sanitizes a given text.
--
-- 1. Removes leading= and trailing whitespace.
-- 2. Replaces consecutive whitespace with a single space character.
--
-- >>> testopV $ sanitize " "
-- Right VEmpty
-- >>> testopV $ sanitize " a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ sanitize "a "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ sanitize " a "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ sanitize " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ sanitize " a b "
-- Right (VText (MkNonEmpty {unpack = "a b"}))
-- >>> testopV $ sanitize " a  b "
-- Right (VText (MkNonEmpty {unpack = "a b"}))
-- >>> testopV $ sanitize "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a b"}))
sanitize :: MonadError HabularaError m => Value -> m Value
sanitize = unaryOperation withText (liftText . T.unwords . T.words)


-- | Lowercases given text.
--
-- >>> testopV $ lower "a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ lower "A"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ lower "aA"
-- Right (VText (MkNonEmpty {unpack = "aa"}))
lower :: MonadError HabularaError m => Value -> m Value
lower = unaryOperation withText (liftText . T.toLower)


-- | Uppercases given text.
--
-- >>> testopV $ upper "a"
-- Right (VText (MkNonEmpty {unpack = "A"}))
-- >>> testopV $ upper "A"
-- Right (VText (MkNonEmpty {unpack = "A"}))
-- >>> testopV $ upper "aA"
-- Right (VText (MkNonEmpty {unpack = "AA"}))
upper :: MonadError HabularaError m => Value -> m Value
upper = unaryOperation withText (liftText . T.toUpper)


-- | Capitalizes given text.
--
-- >>> testopV $ capitalize "a"
-- Right (VText (MkNonEmpty {unpack = "A"}))
-- >>> testopV $ capitalize "A"
-- Right (VText (MkNonEmpty {unpack = "A"}))
-- >>> testopV $ capitalize "aA"
-- Right (VText (MkNonEmpty {unpack = "Aa"}))
-- >>> testopV $ capitalize "aA aA"
-- Right (VText (MkNonEmpty {unpack = "Aa Aa"}))
capitalize :: MonadError HabularaError m => Value -> m Value
capitalize = unaryOperation withText (liftText . T.toTitle)


-- | Appends second value to the first one.
--
-- >>> testopV $ append "a" "b"
-- Right (VText (MkNonEmpty {unpack = "ab"}))
append :: MonadError HabularaError m => Value -> Value -> m Value
append = binaryOperation withText (\x y -> liftText $ (<>) x y)


-- | Appends first value to the second one.
--
-- >>> testopV $ prepend "a" "b"
-- Right (VText (MkNonEmpty {unpack = "ba"}))
prepend :: MonadError HabularaError m => Value -> Value -> m Value
prepend = flip append


-- | Takes first @n@ characters of a given text.
--
-- >>> testopV $ take (number 0) "123456789"
-- Right VEmpty
-- >>> testopV $ take (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "1"}))
-- >>> testopV $ take (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "12"}))
-- >>> testopV $ take (number 100) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}))
take :: MonadError HabularaError m => Value -> Value -> m Value
take x y = withNumber (\n -> withText (liftText . T.take (fromIntegral (floor n :: Integer))) y) x


-- | Takes last @n@ characters of a given text.
--
-- >>> testopV $ takeEnd (number 0) "123456789"
-- Right VEmpty
-- >>> testopV $ takeEnd (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "9"}))
-- >>> testopV $ takeEnd (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "89"}))
-- >>> testopV $ takeEnd (number 100) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}))
takeEnd :: MonadError HabularaError m => Value -> Value -> m Value
takeEnd x y = withNumber (\n -> withText (liftText . T.takeEnd (fromIntegral (floor n :: Integer))) y) x


-- | Drops first @n@ characters of a given text.
--
-- >>> testopV $ drop (number 0) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}))
-- >>> testopV $ drop (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "23456789"}))
-- >>> testopV $ drop (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "3456789"}))
-- >>> testopV $ drop (number 100) "123456789"
-- Right VEmpty
drop :: MonadError HabularaError m => Value -> Value -> m Value
drop x y = withNumber (\n -> withText (liftText . T.drop (fromIntegral (floor n :: Integer))) y) x


-- | Drops last @n@ characters of a given text.
--
-- >>> testopV $ dropEnd (number 0) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}))
-- >>> testopV $ dropEnd (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "12345678"}))
-- >>> testopV $ dropEnd (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "1234567"}))
-- >>> testopV $ dropEnd (number 100) "123456789"
-- Right VEmpty
dropEnd :: MonadError HabularaError m => Value -> Value -> m Value
dropEnd x y = withNumber (\n -> withText (liftText . T.dropEnd (fromIntegral (floor n :: Integer))) y) x


-- | Splits a given text by a given text and returns the element at the given index.
--
-- >>> testopV $ splitIx (number 0) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ splitIx (number 1) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "b"}))
-- >>> testopV $ splitIx (number 2) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "c"}))
-- >>> testopV $ splitIx (number 3) "," "a,b,c"
-- Right VEmpty
splitIx :: MonadError HabularaError m => Value -> Value -> Value -> m Value
splitIx n x y = withNumber (\i -> withText (\s -> withText (liftText . attempt i s) y) x) n
  where
    attempt i s t = fromMaybe "" $ at (floor i) (T.splitOn s t)


-- * Conditionals
--
-- $operatorsConditionals


-- | Checks a condition for a given 'Value' and returns it if the condition
-- holds true, raises given error otherwise.
--
-- >>> testopV $ guard "Expecting a or b" (member (S.fromList ["a", "b"])) "a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ guard "Expecting a or b" (member (S.fromList ["a", "b"])) "c"
-- Left (HabularaErrorOperation "Expecting a or b")
guard :: MonadError HabularaError m => T.Text -> (Value -> m Value) -> Value -> m Value
guard e a v = a v >>= withBool (\x -> if x then pure v else raiseOperationError e)


-- | Checks if given 'Value' is in the given set of 'Value's.
--
-- >>> testopV $ member (S.fromList ["a", "b"]) "a"
-- Right (VBool True)
-- >>> testopV $ member (S.fromList ["a", "b"]) "c"
-- Right (VBool False)
member :: MonadError HabularaError m => S.Set Value -> Value -> m Value
member s v = liftBool $ S.member v s


-- | Checks if given 'Value' is in the given set of 'Value's. If true, returns
-- the 'Value', raises error otherwise.
--
-- >>> testopV $ oneof (S.fromList ["a", "b"]) "a"
-- Right (VText (MkNonEmpty {unpack = "a"}))
-- >>> testopV $ oneof (S.fromList ["a", "b"]) "c"
-- Left (HabularaErrorOperation "Expected one of a,b but received c")
--
-- >>> testopV $ oneof (S.fromList ["a", "b", number 1]) (number 1)
-- Right (VNumber 1.0)
oneof :: MonadError HabularaError m => S.Set Value -> Value -> m Value
oneof s v = guard errmsg (member s) v
  where
    errmsg = "Expected one of " <> T.intercalate "," (display <$> S.toList s) <> " but received " <> display v


-- * Value mappers
--
-- $operatorsValueMappers


-- | Attempts to translate the given 'Value' from a 'Value' dictionary.
--
-- >>> testopV $ translate (M.fromList [("Yes", true), ("No", false)]) "Yes"
-- Right (VBool True)
-- >>> testopV $ translate (M.fromList [("Yes", true), ("No", false)]) "No"
-- Right (VBool False)
-- >>> testopV $ translate (M.fromList [("Yes", true), ("No", false)]) "zartzurt"
-- Left (HabularaErrorOperation "Unexpected value encountered during translation: zartzurt")
translate :: MonadError HabularaError m => M.Map Value Value -> Value -> m Value
translate d v = case M.lookup v d of
  Nothing -> raiseOperationError $ "Unexpected value encountered during translation: " <> display v
  Just tv -> pure tv


-- * Helpers
--
-- $helpers


-- | Provides a unary operation template.
unaryOperation :: MonadError HabularaError m => ((a -> m b) -> Value -> m b) ->  (a -> m b) -> Value -> m b
unaryOperation g = g


-- | Provides a binary operation template.
binaryOperation :: MonadError HabularaError m => ((a -> m b) -> Value -> m b) ->  (a -> a -> m b) -> Value -> Value -> m b
binaryOperation g f x y = g (\a -> g (f a) y) x


-- | Lifts a value into a Value in a monadic context.
liftWith :: Monad m => (a -> Value) -> a -> m Value
liftWith f = pure . f


-- | Lifts a 'Bool' into a Value in a monadic context.
liftBool :: Monad m => Bool -> m Value
liftBool = liftWith VBool


-- | Lifts a 'Day' into a Value in a monadic context.
liftDate :: Monad m => Day -> m Value
liftDate = liftWith VDate


-- | Lifts a 'LocalTime' into a Value in a monadic context.
liftTime :: Monad m => LocalTime -> m Value
liftTime = liftWith VTime


-- | Lifts a 'Scientific' into a Value in a monadic context.
liftNumber :: Monad m => Scientific -> m Value
liftNumber = liftWith VNumber


-- | Lifts a 'T.Text' into a Value in a monadic context.
liftText :: Monad m => T.Text -> m Value
liftText = liftWith text


-- * Utils
--
-- $utils


-- | Attempts to find the element in a given list that is at the given index.
--
-- >>> at 0 ""
-- Nothing
-- >>> at 1 ""
-- Nothing
-- >>> at (-1) ""
-- Nothing
-- >>> at 0 "0123456789"
-- Just '0'
-- >>> at 1 "0123456789"
-- Just '1'
-- >>> at 2 "0123456789"
-- Just '2'
at :: Integer -> [a] -> Maybe a
at _ []     = Nothing
at 0 (x:_)  = Just x
at n (_:xs) = at (n - 1) xs


-- | Convenience function for throwing operation errors.
raiseOperationError :: MonadError HabularaError m => T.Text -> m a
raiseOperationError = throwError . HabularaErrorOperation


-- | Raises a 'HabularaError' indicating that expected type and actual type are differing.
raiseOperationTypeGuard :: MonadError HabularaError m => String -> Value -> m a
raiseOperationTypeGuard expected actual = raiseOperationError . T.pack $ "Expecting '" <> expected <> "', recieved: " <> show actual


-- | Helper function to test operations during development.
testop :: r -> s -> HabularaT r s IO a -> IO (Either HabularaError a)
testop envir state x = fmap fst <$> runHabularaIO envir state x


-- | Helper function to test opreations during development in void, ie. with
-- unit environment and unit state.
testopV :: HabularaT () () IO a -> IO (Either HabularaError a)
testopV = testop () ()
