-- | This module provides primitives and pre-defined convenience functions used
-- to build Habulara 'Record's from raw 'Record's.
--
-- As a quick start:
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaIO)
-- >>> runHabularaIO (HM.fromList [("a", "1")]) (1, HM.empty :: Record) (select "a" >>= asEmpty)
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaIO (HM.fromList [("a", "1")] :: Record) (1, HM.fromList [("b", "1")]) (peek "b" >>= asEmpty)
-- Right (VEmpty,(1,fromList [("b",VText (MkNonEmpty {unpack = "1"}))]))

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Habulara.Core.Operation where

import           Control.Applicative               (Alternative((<|>)))
import           Control.Monad.Except              (MonadError(..))
import           Control.Monad.Reader              (MonadReader)
import           Control.Monad.State               (MonadState)
import           Data.Habulara.Core.Mapping        (OperationEnvar, OperationState, askLabel, getLabel)
import           Data.Habulara.Core.Types.Class    (HabularaError(..), liftMaybe)
import qualified Data.Habulara.Core.Types.NonEmpty as NEV
import           Data.Habulara.Core.Types.Value    (Valuable(..), Value(..))
import           Data.Maybe                        (fromMaybe)
import           Data.Scientific                   (Scientific, toRealFloat)
import qualified Data.Text                         as T
import           Data.Time                         (Day, LocalTime, parseTimeM)
import qualified Data.Time
import           Data.Time.Format                  (defaultTimeLocale)
import           Prelude                           hiding (and, drop, lookup, not, or, subtract, take)
import qualified Prelude


-- $setup
-- >>> import Data.Habulara.Core.Types.Class  (runHabularaIO, runHabularaInVoid)
-- >>> import Data.Habulara.Core.Types.Record (Record)
-- >>> import qualified Data.HashMap.Strict   as HM


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
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty :: Record) (lookup "a")
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty :: Record) (lookup "b")
-- Right (VEmpty,(1,fromList []))
lookup :: (MonadReader OperationEnvar m, MonadError HabularaError m, Alternative m) => Value -> m Value
lookup s = select s <|> pure VEmpty


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- Similar to 'lookup' but if the label does not exist in the record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty :: Record) (select "a")
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty :: Record) (select "b")
-- Left (HabularaErrorOperation "Can not find record field with label: b")
select :: (MonadReader OperationEnvar m, MonadError HabularaError m) => Value -> m Value
select = withText (\s -> askLabel s >>= liftMaybe (HabularaErrorOperation $ "Can not find record field with label: " <> s))


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- state buffer record (record that is currently built up).
--
-- If the label does not exist in the state buffer record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> runHabularaIO () (1, HM.fromList [("a", "A")]) (peek "a")
-- Right (VText (MkNonEmpty {unpack = "A"}),(1,fromList [("a",VText (MkNonEmpty {unpack = "A"}))]))
-- >>> runHabularaIO () (1, HM.fromList [("a", "A")]) (peek "b")
-- Left (HabularaErrorOperation "Can not find buffer record field with label: b")
peek :: (MonadState OperationState m, MonadError HabularaError m) => Value -> m Value
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
-- >>> runHabularaInVoid $ asEmpty VEmpty
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ asEmpty (text " ")
-- Right (VEmpty,())
asEmpty :: Monad m => Value -> m Value
asEmpty = pure . const VEmpty


-- | Attempts to convert the given 'Value' to a 'VBool' value.
--
-- >>> runHabularaInVoid $ asBool VEmpty
-- Right (VBool False,())
-- >>> runHabularaInVoid $ asBool (VText "語")
-- Left (HabularaErrorRead "Can not read Boolean from: \232\170\158")
-- >>> runHabularaInVoid $ asBool (VNumber 42.0)
-- Right (VBool True,())
-- >>> runHabularaInVoid $ asBool (VBool True)
-- Right (VBool True,())
-- >>> runHabularaInVoid $ asBool (VBool False)
-- Right (VBool False,())
-- >>> runHabularaInVoid $ asBool (VDate $ read "2020-12-31")
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VDate 2020-12-31")
-- >>> runHabularaInVoid $ asBool (VTime $ read "2020-12-31 23:59:59")
-- Left (HabularaErrorValueConversion "Can not convert to Boolean: VTime 2020-12-31 23:59:59")
asBool :: MonadError HabularaError m => Value -> m Value
asBool x = bool <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VDate' value.
--
-- >>> runHabularaInVoid $ asDate VEmpty
-- Right (VDate 1858-11-17,())
-- >>> runHabularaInVoid $ asDate (VText "語")
-- Left (HabularaErrorRead "Can not read Date from: \232\170\158")
-- >>> runHabularaInVoid $ asDate (VNumber 42.0)
-- Right (VDate 1858-12-29,())
-- >>> runHabularaInVoid $ asDate (VBool True)
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBool True")
-- >>> runHabularaInVoid $ asDate (VBool False)
-- Left (HabularaErrorValueConversion "Can not convert to Date: VBool False")
-- >>> runHabularaInVoid $ asDate (VDate $ read "2020-12-31")
-- Right (VDate 2020-12-31,())
-- >>> runHabularaInVoid $ asDate (VTime $ read "2020-12-31 23:59:59")
-- Right (VDate 2020-12-31,())
asDate :: MonadError HabularaError m => Value -> m Value
asDate x = date <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VTime' value.
--
-- >>> runHabularaInVoid $ asTime VEmpty
-- Right (VTime 1970-01-01 00:00:00,())
-- >>> runHabularaInVoid $ asTime (VText "語")
-- Left (HabularaErrorRead "Can not read LocalTime from: \232\170\158")
-- >>> runHabularaInVoid $ asTime (VNumber 42.0)
-- Right (VTime 1970-01-01 00:00:42,())
-- >>> runHabularaInVoid $ asTime (VBool True)
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBool True")
-- >>> runHabularaInVoid $ asTime (VBool False)
-- Left (HabularaErrorValueConversion "Can not convert to LocalTime: VBool False")
-- >>> runHabularaInVoid $ asTime (VDate $ read "2020-12-31")
-- Right (VTime 2020-12-31 00:00:00,())
-- >>> runHabularaInVoid $ asTime (VTime $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59,())
asTime :: MonadError HabularaError m => Value -> m Value
asTime x = time <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VNumber' value.
--
-- >>> runHabularaInVoid $ asNumber VEmpty
-- Right (VNumber 0.0,())
-- >>> runHabularaInVoid $ asNumber (VText "語")
-- Left (HabularaErrorRead "Can not read Scientific from: \232\170\158")
-- >>> runHabularaInVoid $ asNumber (VNumber 42.0)
-- Right (VNumber 42.0,())
-- >>> runHabularaInVoid $ asNumber (VBool True)
-- Right (VNumber 1.0,())
-- >>> runHabularaInVoid $ asNumber (VBool False)
-- Right (VNumber 0.0,())
-- >>> runHabularaInVoid $ asNumber (VDate $ read "2020-12-31")
-- Right (VNumber 59214.0,())
-- >>> runHabularaInVoid $ asNumber (VTime $ read "2020-12-31 23:59:59")
-- Right (VNumber 1.609459199e9,())
asNumber :: MonadError HabularaError m => Value -> m Value
asNumber x = number <$> fromValue x


-- | Attempts to convert the given 'Value' to a 'VText' value.
--
-- >>> runHabularaInVoid $ asText VEmpty
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ asText (VText "語")
-- Right (VText (MkNonEmpty {unpack = "\35486"}),())
-- >>> runHabularaInVoid $ asText (VNumber 42.0)
-- Right (VText (MkNonEmpty {unpack = "42.0"}),())
-- >>> runHabularaInVoid $ asText (VBool True)
-- Right (VText (MkNonEmpty {unpack = "True"}),())
-- >>> runHabularaInVoid $ asText (VBool False)
-- Right (VText (MkNonEmpty {unpack = "False"}),())
-- >>> runHabularaInVoid $ asText (VDate $ read "2020-12-31")
-- Right (VText (MkNonEmpty {unpack = "2020-12-31"}),())
-- >>> runHabularaInVoid $ asText (VTime $ read "2020-12-31 23:59:59")
-- Right (VText (MkNonEmpty {unpack = "2020-12-31 23:59:59"}),())
asText :: MonadError HabularaError m => Value -> m Value
asText x = text <$> fromValue x


-- * Value Subtype Guarded Operators
--
-- $ operatorsValueSubtypeGuards


-- | Runs the action if the value is a 'VEmpty', raises error otherwise.
--
-- >>> runHabularaInVoid $ withEmpty (pure true) VEmpty
-- Right (VBool True,())
-- >>> runHabularaInVoid $ withEmpty (pure true) true
-- Left (HabularaErrorOperation "Expecting 'VEmpty', recieved: VBool True")
withEmpty :: MonadError HabularaError m => m a -> Value -> m a
withEmpty a VEmpty = a
withEmpty _ v      = raiseOperationTypeGuard "VEmpty" v


-- | Runs the action if the value is a 'VBool', raises error otherwise.
--
-- >>> runHabularaInVoid $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) true
-- Right (VText (MkNonEmpty {unpack = "Yes"}),())
-- >>> runHabularaInVoid $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) false
-- Right (VText (MkNonEmpty {unpack = "No"}),())
-- >>> runHabularaInVoid $ withBool (\x -> pure $ if x then (text "Yes") else (text "No")) VEmpty
-- Left (HabularaErrorOperation "Expecting 'VBool', recieved: VEmpty")
withBool :: MonadError HabularaError m => (Bool -> m a) -> Value -> m a
withBool f (VBool x) = f x
withBool _ v         = raiseOperationTypeGuard "VBool" v


-- | Runs the action if the value is a 'VDate', raises error otherwise.
--
-- >>> runHabularaInVoid $ withDate (\x -> pure $ "Today is " <> show x) (VDate $ read "2020-12-31")
-- Right ("Today is 2020-12-31",())
-- >>> runHabularaInVoid $ withDate (\x -> pure $ "Today is " <> show x) true
-- Left (HabularaErrorOperation "Expecting 'VDate', recieved: VBool True")
withDate :: MonadError HabularaError m => (Day -> m a) -> Value -> m a
withDate f (VDate x) = f x
withDate _ v         = raiseOperationTypeGuard "VDate" v


-- | Runs the action if the value is a 'VTime', raises error otherwise.
--
-- >>> runHabularaInVoid $ withTime (\x -> pure $ "Now is " <> show x) (VTime $ read "2020-12-31 23:59:59")
-- Right ("Now is 2020-12-31 23:59:59",())
-- >>> runHabularaInVoid $ withTime (\x -> pure $ "Now is " <> show x) true
-- Left (HabularaErrorOperation "Expecting 'VTime', recieved: VBool True")
withTime :: MonadError HabularaError m => (LocalTime -> m a) -> Value -> m a
withTime f (VTime x) = f x
withTime _ v         = raiseOperationTypeGuard "VTime" v


-- | Runs the action if the value is a 'VNumber', raises error otherwise.
--
-- >>> runHabularaInVoid $ withNumber (pure . (1 +)) (VNumber 42)
-- Right (43.0,())
-- >>> runHabularaInVoid $ withNumber (pure . (1 +)) true
-- Left (HabularaErrorOperation "Expecting 'VNumber', recieved: VBool True")
withNumber :: MonadError HabularaError m => (Scientific -> m a) -> Value -> m a
withNumber f (VNumber x) = f x
withNumber _ v           = raiseOperationTypeGuard "VNumber" v


-- | Runs the action if the value is a 'VText', raises error otherwise.
--
-- >>> runHabularaInVoid $ withText (pure . ("Hello " <>)) "World"
-- Right ("Hello World",())
-- >>> runHabularaInVoid $ withText (pure . ("Hello " <>)) true
-- Left (HabularaErrorOperation "Expecting 'VText', recieved: VBool True")
-- >>> runHabularaIO (HM.fromList [("a", "A"), ("b", "B")]) () $ select "a"
-- Right (VText (MkNonEmpty {unpack = "A"}),())
withText :: MonadError HabularaError m => (T.Text -> m a) -> Value -> m a
withText f (VText x) = f . NEV.unpack $ x
withText _ v         = raiseOperationTypeGuard "VText" v


-- * Boolean Operators
--
-- $operatorsBoolean


-- | Boolean @not@ operation.
--
-- >>> runHabularaInVoid $ not false
-- Right (VBool True,())
-- >>> runHabularaInVoid $ not true
-- Right (VBool False,())
not :: MonadError HabularaError m => Value -> m Value
not = unaryOperation withBool (liftBool . Prelude.not)


-- | Boolean @and@ operation.
--
-- >>> runHabularaInVoid $ and false false
-- Right (VBool False,())
-- >>> runHabularaInVoid $ and false true
-- Right (VBool False,())
-- >>> runHabularaInVoid $ and true false
-- Right (VBool False,())
-- >>> runHabularaInVoid $ and true true
-- Right (VBool True,())
and :: MonadError HabularaError m => Value -> Value -> m Value
and = binaryOperation withBool (\x y -> liftBool $ (&&) x y)


-- | Boolean @or@ operation.
--
-- >>> runHabularaInVoid $ or false false
-- Right (VBool False,())
-- >>> runHabularaInVoid $ or false true
-- Right (VBool True,())
-- >>> runHabularaInVoid $ or true false
-- Right (VBool True,())
-- >>> runHabularaInVoid $ or true true
-- Right (VBool True,())
or :: MonadError HabularaError m => Value -> Value -> m Value
or = binaryOperation withBool (\x y -> liftBool $ (||) x y)


-- | Boolean @xor@ operation.
--
-- >>> runHabularaInVoid $ xor false false
-- Right (VBool False,())
-- >>> runHabularaInVoid $ xor false true
-- Right (VBool True,())
-- >>> runHabularaInVoid $ xor true false
-- Right (VBool True,())
-- >>> runHabularaInVoid $ xor true true
-- Right (VBool False,())
xor :: MonadError HabularaError m => Value -> Value -> m Value
xor = binaryOperation withBool (\x y -> liftBool $ (/=) x y)


-- * Date Operators
--
-- $operatorsDate


-- | Parses a date.
--
-- >>> runHabularaInVoid $ parseDate "%Y%m%d" "20201231"
-- Right (VDate 2020-12-31,())
-- >>> runHabularaInVoid $ parseDate "%Y%m%d" "2020-12-31"
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
-- >>> runHabularaInVoid $ addDays (number 0) (date $ read "2020-12-31")
-- Right (VDate 2020-12-31,())
-- >>> runHabularaInVoid $ addDays (number 1) (date $ read "2020-12-31")
-- Right (VDate 2021-01-01,())
-- >>> runHabularaInVoid $ addDays (number (-1)) (date $ read "2020-12-31")
-- Right (VDate 2020-12-30,())
addDays :: MonadError HabularaError m => Value -> Value -> m Value
addDays x y = withNumber (\n -> withDate (liftDate . Data.Time.addDays (floor n)) y) x


-- * Time Operators
--
-- $operatorsTime


-- | Parses a time.
--
-- >>> runHabularaInVoid $ parseTime "%Y%m%d%H%M%S" "20201231235959"
-- Right (VTime 2020-12-31 23:59:59,())
-- >>> runHabularaInVoid $ parseTime "%Y%m%d%H%M%S" "2020-12-31 23:59:59"
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
-- >>> runHabularaInVoid $ addSeconds (number 0) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59,())
-- >>> runHabularaInVoid $ addSeconds (number 0.9) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:59.9,())
-- >>> runHabularaInVoid $ addSeconds (number 1) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2021-01-01 00:00:00,())
-- >>> runHabularaInVoid $ addSeconds (number (-1)) (time $ read "2020-12-31 23:59:59")
-- Right (VTime 2020-12-31 23:59:58,())
addSeconds :: MonadError HabularaError m => Value -> Value -> m Value
addSeconds x y = withNumber (\n -> withTime (liftTime . Data.Time.addLocalTime (fromRational $ toRational n)) y) x


-- * Numeric Operators
--
-- $operatorsNumeric


-- | Addition operation.
--
-- >>> runHabularaInVoid $ add (VNumber 41) (VNumber 1)
-- Right (VNumber 42.0,())
add :: MonadError HabularaError m => Value -> Value -> m Value
add = binaryOperation withNumber (\x y -> liftNumber $ (+) x y)


-- | Subtraction operation.
--
-- >>> runHabularaInVoid $ subtract (VNumber 43) (VNumber 1)
-- Right (VNumber 42.0,())
-- >>> runHabularaInVoid $ flip subtract (VNumber 43) (VNumber 1)
-- Right (VNumber -42.0,())
subtract :: MonadError HabularaError m => Value -> Value -> m Value
subtract = binaryOperation withNumber (\x y -> liftNumber $ (-) x y)


-- | Multiplication operation.
--
-- >>> runHabularaInVoid $ multiply (VNumber 6) (VNumber 7)
-- Right (VNumber 42.0,())
multiply :: MonadError HabularaError m => Value -> Value -> m Value
multiply = binaryOperation withNumber (\x y -> liftNumber $ (*) x y)


-- | Division operation.
--
-- >>> runHabularaInVoid $ divide (VNumber 84) (VNumber 2)
-- Right (VNumber 42.0,())
-- >>> runHabularaInVoid $ flip divide (VNumber 84) (VNumber 2)
-- Right (VNumber 2.3809523809523808e-2,())
divide :: MonadError HabularaError m => Value -> Value -> m Value
divide = binaryOperation withNumber (\x y -> liftNumber $ floatingDiv x y)
  where
    floatingDiv :: Scientific -> Scientific -> Scientific  -- TODO: Find a better solution
    floatingDiv x y = read . show $ (toRealFloat x :: Double) / (toRealFloat y :: Double)


-- | Converts a number to percentage points.
-- >>> runHabularaInVoid $ percentage (VNumber 0)
-- Right (VNumber 0.0,())
-- >>> runHabularaInVoid $ percentage (VNumber 0.5)
-- Right (VNumber 50.0,())
-- >>> runHabularaInVoid $ percentage (VNumber 1)
-- Right (VNumber 100.0,())
-- >>> runHabularaInVoid $ percentage true
-- Left (HabularaErrorOperation "Expecting 'VNumber', recieved: VBool True")
percentage :: MonadError HabularaError m => Value -> m Value
percentage = unaryOperation withNumber (liftNumber . (*) 100)


-- * Textual Operators
--
-- $operatorsTextual


-- | Strips whitespace from the beginning and end of a textual value.
--
-- >>> runHabularaInVoid $ trim " "
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ trim " a"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trim "a "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trim " a "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trim " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trim " a b "
-- Right (VText (MkNonEmpty {unpack = "a b"}),())
-- >>> runHabularaInVoid $ trim " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b"}),())
-- >>> runHabularaInVoid $ trim "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b"}),())
trim :: MonadError HabularaError m => Value -> m Value
trim = unaryOperation withText (liftText . T.strip)


-- | Strips whitespace from the beginning of a textual value.
--
-- >>> runHabularaInVoid $ trimStart " "
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ trimStart " a"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trimStart "a "
-- Right (VText (MkNonEmpty {unpack = "a "}),())
-- >>> runHabularaInVoid $ trimStart " a "
-- Right (VText (MkNonEmpty {unpack = "a "}),())
-- >>> runHabularaInVoid $ trimStart " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n "}),())
-- >>> runHabularaInVoid $ trimStart " a b "
-- Right (VText (MkNonEmpty {unpack = "a b "}),())
-- >>> runHabularaInVoid $ trimStart " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b "}),())
-- >>> runHabularaInVoid $ trimStart "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b \t\r\n "}),())
trimStart :: MonadError HabularaError m => Value -> m Value
trimStart = unaryOperation withText (liftText . T.stripStart)


-- | Strips whitespace from the end of a textual value.
--
-- >>> runHabularaInVoid $ trimEnd " "
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ trimEnd " a"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ trimEnd "a "
-- Right (VText (MkNonEmpty {unpack = "a "}),())
-- >>> runHabularaInVoid $ trimEnd " a "
-- Right (VText (MkNonEmpty {unpack = "a "}),())
-- >>> runHabularaInVoid $ trimEnd " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n "}),())
-- >>> runHabularaInVoid $ trimEnd " a b "
-- Right (VText (MkNonEmpty {unpack = "a b "}),())
-- >>> runHabularaInVoid $ trimEnd " a  b "
-- Right (VText (MkNonEmpty {unpack = "a  b "}),())
-- >>> runHabularaInVoid $ trimEnd "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a \t\r\n b \t\r\n "}),())
trimEnd :: MonadError HabularaError m => Value -> m Value
trimEnd = unaryOperation withText (liftText . T.stripStart)


-- | Sanitizes a given text.
--
-- 1. Removes leading= and trailing whitespace.
-- 2. Replaces consecutive whitespace with a single space character.
--
-- >>> runHabularaInVoid $ sanitize " "
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ sanitize " a"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ sanitize "a "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ sanitize " a "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ sanitize " \t\r\n a \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ sanitize " a b "
-- Right (VText (MkNonEmpty {unpack = "a b"}),())
-- >>> runHabularaInVoid $ sanitize " a  b "
-- Right (VText (MkNonEmpty {unpack = "a b"}),())
-- >>> runHabularaInVoid $ sanitize "\t\r\n a \t\r\n b \t\r\n "
-- Right (VText (MkNonEmpty {unpack = "a b"}),())
sanitize :: MonadError HabularaError m => Value -> m Value
sanitize = unaryOperation withText (liftText . T.unwords . T.words)


-- | Lowercases given text.
--
-- >>> runHabularaInVoid $ lower "a"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ lower "A"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ lower "aA"
-- Right (VText (MkNonEmpty {unpack = "aa"}),())
lower :: MonadError HabularaError m => Value -> m Value
lower = unaryOperation withText (liftText . T.toLower)


-- | Uppercases given text.
--
-- >>> runHabularaInVoid $ upper "a"
-- Right (VText (MkNonEmpty {unpack = "A"}),())
-- >>> runHabularaInVoid $ upper "A"
-- Right (VText (MkNonEmpty {unpack = "A"}),())
-- >>> runHabularaInVoid $ upper "aA"
-- Right (VText (MkNonEmpty {unpack = "AA"}),())
upper :: MonadError HabularaError m => Value -> m Value
upper = unaryOperation withText (liftText . T.toUpper)


-- | Capitalizes given text.
--
-- >>> runHabularaInVoid $ capitalize "a"
-- Right (VText (MkNonEmpty {unpack = "A"}),())
-- >>> runHabularaInVoid $ capitalize "A"
-- Right (VText (MkNonEmpty {unpack = "A"}),())
-- >>> runHabularaInVoid $ capitalize "aA"
-- Right (VText (MkNonEmpty {unpack = "Aa"}),())
-- >>> runHabularaInVoid $ capitalize "aA aA"
-- Right (VText (MkNonEmpty {unpack = "Aa Aa"}),())
capitalize :: MonadError HabularaError m => Value -> m Value
capitalize = unaryOperation withText (liftText . T.toTitle)


-- | Appends second value to the first one.
--
-- >>> runHabularaInVoid $ append "a" "b"
-- Right (VText (MkNonEmpty {unpack = "ab"}),())
append :: MonadError HabularaError m => Value -> Value -> m Value
append = binaryOperation withText (\x y -> liftText $ (<>) x y)


-- | Appends first value to the second one.
--
-- >>> runHabularaInVoid $ prepend "a" "b"
-- Right (VText (MkNonEmpty {unpack = "ba"}),())
prepend :: MonadError HabularaError m => Value -> Value -> m Value
prepend = flip append


-- | Takes first @n@ characters of a given text.
--
-- >>> runHabularaInVoid $ take (number 0) "123456789"
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ take (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "1"}),())
-- >>> runHabularaInVoid $ take (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "12"}),())
-- >>> runHabularaInVoid $ take (number 100) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}),())
take :: MonadError HabularaError m => Value -> Value -> m Value
take x y = withNumber (\n -> withText (liftText . T.take (fromIntegral (floor n :: Integer))) y) x


-- | Takes last @n@ characters of a given text.
--
-- >>> runHabularaInVoid $ takeEnd (number 0) "123456789"
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ takeEnd (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "9"}),())
-- >>> runHabularaInVoid $ takeEnd (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "89"}),())
-- >>> runHabularaInVoid $ takeEnd (number 100) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}),())
takeEnd :: MonadError HabularaError m => Value -> Value -> m Value
takeEnd x y = withNumber (\n -> withText (liftText . T.takeEnd (fromIntegral (floor n :: Integer))) y) x


-- | Drops first @n@ characters of a given text.
--
-- >>> runHabularaInVoid $ drop (number 0) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}),())
-- >>> runHabularaInVoid $ drop (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "23456789"}),())
-- >>> runHabularaInVoid $ drop (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "3456789"}),())
-- >>> runHabularaInVoid $ drop (number 100) "123456789"
-- Right (VEmpty,())
drop :: MonadError HabularaError m => Value -> Value -> m Value
drop x y = withNumber (\n -> withText (liftText . T.drop (fromIntegral (floor n :: Integer))) y) x


-- | Drops last @n@ characters of a given text.
--
-- >>> runHabularaInVoid $ dropEnd (number 0) "123456789"
-- Right (VText (MkNonEmpty {unpack = "123456789"}),())
-- >>> runHabularaInVoid $ dropEnd (number 1) "123456789"
-- Right (VText (MkNonEmpty {unpack = "12345678"}),())
-- >>> runHabularaInVoid $ dropEnd (number 2) "123456789"
-- Right (VText (MkNonEmpty {unpack = "1234567"}),())
-- >>> runHabularaInVoid $ dropEnd (number 100) "123456789"
-- Right (VEmpty,())
dropEnd :: MonadError HabularaError m => Value -> Value -> m Value
dropEnd x y = withNumber (\n -> withText (liftText . T.dropEnd (fromIntegral (floor n :: Integer))) y) x


-- | Splits a given text by a given text and returns the element at the given index.
--
-- >>> runHabularaInVoid $ splitIx (number 0) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "a"}),())
-- >>> runHabularaInVoid $ splitIx (number 1) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "b"}),())
-- >>> runHabularaInVoid $ splitIx (number 2) "," "a,b,c"
-- Right (VText (MkNonEmpty {unpack = "c"}),())
-- >>> runHabularaInVoid $ splitIx (number 3) "," "a,b,c"
-- Right (VEmpty,())
splitIx :: MonadError HabularaError m => Value -> Value -> Value -> m Value
splitIx n x y = withNumber (\i -> withText (\s -> withText (liftText . attempt i s) y) x) n
  where
    attempt i s t = fromMaybe "" $ at (floor i) (T.splitOn s t)


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
