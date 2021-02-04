{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

module Data.Habulara.Core.Mapping where

import           Control.Monad.Except       (MonadError(throwError))
import           Control.Monad.IO.Class     (MonadIO(..))
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.State        (MonadState(..), gets, modify')
import           Data.Habulara.Core         (HabularaT, Label, MonadHabulara, Record, Value, runHabularaT)
import qualified Data.HashMap.Strict        as HM


-- * Types
--
-- In essence, an record or value mapper is defined in a 'MonadHabulara' context
-- ('MapperM') with an environment 'MapperEnvir' and state 'MapperState'.
--
-- $types


-- | 'MonadHabulara' constraint for record or value mapping.
type MapperM m = MonadHabulara MapperEnvir MapperState m


-- | Environment type for record or value 'MapperM'.
--
-- This is a 2-tuple of current record number and the raw record data.
type MapperEnvir = (Integer, Record)


-- | State type for record or value 'MapperM'.
--
-- This is the current record being built.
type MapperState = Record


-- | High-level value mapper type based on 'HabularaT' in an 'IO' monad.
--
-- Library users may still wish to stick to 'MapperM' constraint when
-- defining value mappers.
type ValueMapperT = HabularaT MapperEnvir MapperState IO Value


-- | Type definition of a field mapper specification.
--
-- Essentially, this aids a mapping from the label of the field to the value
-- mapper of the field.
type FieldMapper = (Label, ValueMapperT)


-- * Mapping
--
-- $mapping


-- | Maps a 'Record' to a 'Record' as per given list of 'FieldMapper' values
-- (field label - field value mapping).
--
-- This function is constrained by 'MonadHabulara' with an 'Integer' state
-- whereby the state stands for the current record number.
mapRecord :: MonadHabulara r Integer m => [FieldMapper] -> Record -> m Record
mapRecord ops record = do
  nenvar <- gets (, record)
  result <- liftIO $ runHabularaT nenvar HM.empty (buildRecord ops)
  either throwError (pure . fst) result


-- | Builds a 'Record' within the 'Operation' monad as per given @'Label' ->
-- 'Value'@ mapping.
--
-- The constraint's monad is parameterized, hence the 'm' in the mapping
-- (@'Label', m 'Value'@). Call-sites may consider using 'mapRecord' which is a
-- high level function using this function and constrained by 'MapperM'
-- providing a state monad for the record number being mapped.
buildRecord :: MapperM m => [(Label, m Value)] -> m Record
buildRecord []             = get
buildRecord ((l, op) : xs) = op >>= modify' . HM.insert l >> buildRecord xs


-- * Helpers
--
-- $helpers


-- | Attempts to find the value associated with the given label in the operation
-- environment and applies the given function to it.
asksLabel :: MonadReader MapperEnvir m => Label -> (Value -> a) -> m (Maybe a)
asksLabel l f = asks (fmap f . HM.lookup l . snd)


-- | Attempts to find the value associated with the given label in the operation
-- environment.
askLabel :: MonadReader MapperEnvir m => Label -> m (Maybe Value)
askLabel l = asksLabel l id


-- | Returns the number of the current record mapped.
askCount :: MonadReader MapperEnvir m => m Integer
askCount = asks fst


-- | Attempts to find the value associated with the given label in the state
-- buffer record and applies the given function to it.
getsLabel :: MonadState MapperState m => Label -> (Value -> a) -> m (Maybe a)
getsLabel l f = gets (fmap f . HM.lookup l)


-- | Attempts to find the value associated with the given label in the state
-- buffer record.
getLabel :: MonadState MapperState m => Label -> m (Maybe Value)
getLabel l = getsLabel l id
