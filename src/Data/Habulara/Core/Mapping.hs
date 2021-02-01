{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

module Data.Habulara.Core.Mapping where

import           Control.Monad.Except       (MonadError(throwError))
import           Control.Monad.IO.Class     (MonadIO(..))
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.State        (MonadState(..), gets)
import           Data.Habulara.Core         (HabularaT, Label, MonadHabulara, Record, Value, runHabularaT)
import qualified Data.HashMap.Strict        as HM

-- * Types
--
-- In essence, an operation is defined in a 'MonadHabulara' context
-- ('Operation') with a raw 'Record' as the environment variable
-- ('OperationEnvar') and a 2-tuple of (1) current row number and (2) the buffer
-- 'Record' being built up ('OperationState').
--
-- $types


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


-- | High-level operator type based on 'HabularaM'.
--
-- Library users may still wish to stick to 'Operation m' constraint when
-- defining operators.
type Operator = HabularaT OperationEnvar OperationState IO Value


-- | Type definition of a field operator.
--
-- Essentially, this aids a mapping from the label of the field to the operator
-- of the field.
type FieldOperator = (Label, Operator)


-- | Type definition for the mapping monad based on 'MonadHabulara'.
--
-- We are keeping the __current__ row number as the state.
type MappingM m = MonadHabulara () Integer m


-- * Mapping
--
-- $mapping


-- | Maps a 'Record' to a 'Record' as per given 'FieldOperator's (field label -
-- field value mapping).
--
-- This function is constrained by 'MappingM', ie. 'MonadHabulara' with no
-- environment but a state representing the current record counter as an
-- 'Integer'.
mapRecord :: MappingM m => [FieldOperator] -> Record -> m Record
mapRecord ops record = do
  nstate <- gets (, HM.empty)
  result <- liftIO $ runHabularaT record nstate (buildRecord ops)
  either throwError (pure . fst) result


-- | Builds a 'Record' within the 'Operation' monad as per given @'Label' ->
-- 'Value'@ mapping.
--
-- The constraint's monad is parameterized, hence the 'm' in the mapping
-- (@'Label', m 'Value'@). Call-sites may consider using 'mapRecord' which is a
-- high level function using this function and constrained by 'MappingM'
-- providing a state monad for the record number being mapped.
buildRecord :: Operation m => [(Label, m Value)] -> m Record
buildRecord []                  = gets snd
buildRecord ((name, prog) : xs) = prog >>= modifyRecord . HM.insert name >> buildRecord xs


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
