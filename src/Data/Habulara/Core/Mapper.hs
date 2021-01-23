{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Core.Mapper where


import           Control.Applicative      (Alternative((<|>)))
import           Control.Monad.Except     (MonadError(throwError))
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState(get, put), gets)
import           Data.Habulara.Core.Class (HabularaError(HabularaErrorOperation), MonadHabulara)
import           Data.Habulara.Core.Types (Label, Record, Value(VEmpty))
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T


-- | 'MonadHabulara' constraint for operations.
type Operation m = MonadHabulara OperationEnvar OperationState m


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


-- | Lifts a 'Maybe' into operation context with a default error message.
liftMaybe :: MonadError HabularaError m => T.Text -> Maybe Value -> m Value
liftMaybe err = maybe (raiseOperationError err) pure


-- | Convenience function for throwing operation errors.
raiseOperationError :: MonadError HabularaError m => T.Text -> m a
raiseOperationError = throwError . HabularaErrorOperation


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- If the label does not exist in the record, 'VEmpty' is returned.
--
-- >>> import Data.Habulara.Core.Class (runHabularaT)
-- >>> runHabularaT (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select "a") :: IO (Either HabularaError (Value, OperationState))
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaT (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select "b") :: IO (Either HabularaError (Value, OperationState))
-- Right (VEmpty,(1,fromList []))
select :: Operation m => T.Text -> m Value
select s = select' s <|> pure VEmpty


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- raw record.
--
-- Similar to 'select' but if the label does not exist in the record,
-- 'HabularaErrorOperation' is raised instead.
--
-- >>> import Data.Habulara.Core.Class (runHabularaT)
-- >>> runHabularaT (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select' "a") :: IO (Either HabularaError (Value, OperationState))
-- Right (VEmpty,(1,fromList []))
-- >>> runHabularaT (HM.fromList [("a", VEmpty)]) (1, HM.empty) (select' "b") :: IO (Either HabularaError (Value, OperationState))
-- Left (HabularaErrorOperation "Can not find record field with label: b")
select' :: Operation m => T.Text -> m Value
select' s = askLabel s >>= liftMaybe ("Can not find record field with label: " <> s)


-- | Attempts to retrieve the field 'Value' for the given field label from the
-- state buffer record.
--
-- If the label does not exist in the state buffer record,
-- 'HabularaErrorOperation' is raised instead.
use :: Operation m => T.Text -> m Value
use s = getLabel s >>= liftMaybe ("Can not find buffer record field with label: " <> s)


-- | Performs a raw record mapping with the given list of tuples of field label
-- and field value operator.
--
-- >>> import Data.Habulara.Core.Class (runHabularaT)
-- >>> runHabularaT (HM.fromList [("a", VEmpty), ("b", VEmpty)]) (1, HM.empty) (mapRecord [("A", select "a"), ("B", select "b")]) :: IO (Either HabularaError (Record, OperationState))
-- Right (fromList [("A",VEmpty),("B",VEmpty)],(1,fromList [("A",VEmpty),("B",VEmpty)]))
mapRecord :: Operation m => [(Label, m Value)] -> m Record
mapRecord []                  = gets snd
mapRecord ((name, prog) : xs) = prog >>= modifyRecord . HM.insert name >> mapRecord xs
