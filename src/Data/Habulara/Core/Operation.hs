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

import           Control.Applicative             (Alternative((<|>)))
import           Control.Monad.Except            (MonadError(throwError))
import           Control.Monad.Reader            (MonadReader, asks)
import           Control.Monad.State             (MonadState(get, put), gets)
import           Data.Habulara.Core.Types.Class  (HabularaError(..), MonadHabulara, liftMaybe)
import           Data.Habulara.Core.Types.Record (Label, Record)
import           Data.Habulara.Core.Types.Value  (Valuable(..), Value(..))
import qualified Data.HashMap.Strict             as HM
import           Data.Scientific                 (Scientific)
import qualified Data.Text                       as T
import           Data.Time                       (Day, LocalTime)
import           Prelude                         hiding (lookup)

-- * Types
--
-- $types
--
-- In essence, an operation is defined in a 'MonadHabulara' context
-- ('Operation') with a raw 'Record' as the environment variable
-- ('OperationEnvar') and a 2-tuple of (1) current row number and (2) the buffer
-- 'Record' being built up ('OperationState').


-- | Environment type for 'Operation'.
--
-- This is the raw row record as the input to the current operation.
type OperationEnvar = Record


-- | State type for 'Operator'.
--
-- This is a 2-tuple of:
--
-- 1. Current row number in operation, and
-- 2. Current buffer record being built up.
type OperationState = (Integer, Record)


-- | 'MonadHabulara' constraint for operations.
type Operation m = MonadHabulara OperationEnvar OperationState m


-- * Operations
--
-- $operations


-- ** Field Accessors
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
-- >>> import Data.Habulara.Core.Types.Class (runHabularaIO)
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty) (lookup "a")
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty) (lookup "b")
-- Right (VEmpty,(1,fromList []))
lookup :: (MonadReader OperationEnvar m, MonadError HabularaError m, Alternative m) => T.Text -> m Value
lookup s = select s <|> pure VEmpty


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- Similar to 'lookup' but if the label does not exist in the record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaIO)
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select "a")
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaIO (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select "b")
-- Left (HabularaErrorOperation "Can not find record field with label: b")
select :: (MonadReader OperationEnvar m, MonadError HabularaError m) => T.Text -> m Value
select s = askLabel s >>= liftMaybe (HabularaErrorOperation $ "Can not find record field with label: " <> s)


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- state buffer record (record that is currently built up).
--
-- If the label does not exist in the state buffer record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaIO)
-- >>> runHabularaIO () (1, HM.fromList [("a", "A")]) (peek "a")
-- Right (VText (MkNonEmpty {unpack = "A"}),(1,fromList [("a",VText (MkNonEmpty {unpack = "A"}))]))
-- >>> runHabularaIO () (1, HM.fromList [("a", "A")]) (peek "b")
-- Left (HabularaErrorOperation "Can not find buffer record field with label: b")
peek :: (MonadState OperationState m, MonadError HabularaError m) => T.Text -> m Value
peek s = getLabel s >>= liftMaybe (HabularaErrorOperation $ "Can not find buffer record field with label: " <> s)


-- ** Value Constructors
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


-- ** Value Converters
--
-- These operations are primitives for 'Value' conversions.
--
-- $operationsValueConverters


-- | Converts the value to a 'VEmpty' value.
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
-- >>> runHabularaInVoid $ asEmpty VEmpty
-- Right (VEmpty,())
-- >>> runHabularaInVoid $ asEmpty (text " ")
-- Right (VEmpty,())
asEmpty :: Monad m => Value -> m Value
asEmpty = pure . const VEmpty


-- | Attempts to convert the given 'Value' to a 'VBool' value.
--
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
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
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
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
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
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
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
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
-- >>> import Data.Habulara.Core.Types.Class (runHabularaInVoid)
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


-- * Helpers
--
-- $helpers

-- | Attempts to find the value associated with the given label in the operation
-- environment and applies the given function to it.
asksLabel :: MonadReader OperationEnvar m => Label -> (Value -> a) -> m (Maybe a)
asksLabel l f = asks (fmap f . HM.lookup l)


-- | Attempts to find the value associated with the given label in the operation
-- environment.
askLabel :: MonadReader OperationEnvar m => Label -> m (Maybe Value)
askLabel l = asksLabel l id


-- | Attempts to find the value associated with the given label in the state
-- buffer record and applies the given function to it.
getsLabel :: MonadState OperationState m => Label -> (Value -> a) -> m (Maybe a)
getsLabel l f = gets (fmap f . HM.lookup l . snd)


-- | Attempts to find the value associated with the given label in the state
-- buffer record.
getLabel :: MonadState OperationState m => Label -> m (Maybe Value)
getLabel l = getsLabel l id


-- | Modifies the operation state buffer record.
modifyRecord :: MonadState OperationState m => (Record -> Record) -> m ()
modifyRecord f = get >>= (\(c, r) -> put (c, f r))


-- | Convenience function for throwing operation errors.
raiseOperationError :: MonadError HabularaError m => T.Text -> m a
raiseOperationError = throwError . HabularaErrorOperation
