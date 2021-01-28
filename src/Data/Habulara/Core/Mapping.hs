{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

module Data.Habulara.Core.Mapping where

import           Control.Monad.Except         (MonadError(throwError))
import           Control.Monad.State          (MonadIO(..), gets)
import           Data.Habulara.Core.Class     (HabularaT, MonadHabulara, runHabularaT)
import           Data.Habulara.Core.Operation (Operation, OperationEnvar, OperationState, modifyRecord)
import           Data.Habulara.Core.Types     (Label, Record, Value)
import qualified Data.HashMap.Strict          as HM


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
