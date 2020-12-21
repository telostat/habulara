{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Mapping where

import qualified Control.Monad.Except   as EM
import           Control.Monad.Identity (Identity(runIdentity))
import           Control.Monad.Reader   (asks)
import qualified Control.Monad.State    as SM
import           Data.Habulara.Types
                 ( DynamicContext(..)
                 , Field
                 , FieldMapper
                 , FieldMappers
                 , Record
                 , StaticContext(..)
                 , Value(..)
                 , ValueOperator
                 , runREST
                 )
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T


-- | Maps a single field.
mapField :: StaticContext -> Record -> FieldMapper -> Either String (Field, Value)
mapField ctx rec = retcase . runIdentity . runREST (MkDynamicContext "" VEmpty rec) ctx
  where
    retcase = \case
      (Left err, _)                     -> Left err
      (Right v, MkDynamicContext f _ _) -> Right (f, v)


-- | Maps an entire record.
mapRecord :: Record -> FieldMappers -> Either String Record
mapRecord raw fms = mapRecordAux (MkStaticContext raw) fms HM.empty
  where
    mapRecordAux :: StaticContext -> FieldMappers -> Record -> Either String Record
    mapRecordAux _ [] acc       = Right acc
    mapRecordAux ctx (x:xs) acc = case mapField ctx acc x of
      Left err     -> Left err
      Right (f, v) -> mapRecordAux ctx xs (HM.insert f v acc)


(~>) :: FieldMapper -> ValueOperator -> FieldMapper
mf ~> mv = mf >> liftVO mv
infixr 1 ~>


select :: Field -> FieldMapper
select f = asks (HM.lookup f . staticContextRecord) >>= \case
  Nothing -> EM.throwError $ "No such field found in the raw record: " <> T.unpack f
  Just fv -> setFieldValue f fv


selectAs :: Field -> Field -> FieldMapper
selectAs f a = asks (HM.lookup f . staticContextRecord) >>= \case
  Nothing -> EM.throwError $ "No such field found in the raw record: " <> T.unpack f
  Just fv -> setFieldValue a fv


use :: Field -> FieldMapper
use f = SM.gets (HM.lookup f . dynamicContextBuffer) >>= \case
  Nothing -> EM.throwError $ "No such field found in the buffer record: " <> T.unpack f
  Just fv -> setFieldValue f fv


useAs :: Field -> Field -> FieldMapper
useAs f n = SM.gets (HM.lookup f . dynamicContextBuffer) >>= \case
  Nothing -> EM.throwError $ "No such field found in the buffer record: " <> T.unpack f
  Just fv -> setFieldValue n fv


rename :: Field -> FieldMapper
rename = setField


setField :: Field -> FieldMapper
setField f = SM.modify (\dc -> dc {dynamicContextField = f}) >> SM.gets dynamicContextValue


setValue :: Value -> FieldMapper
setValue v = SM.modify (\dc -> dc {dynamicContextValue = v}) >> return v


setFieldValue :: Field -> Value -> FieldMapper
setFieldValue f v = SM.modify (\dc -> dc {dynamicContextField = f, dynamicContextValue = v}) >> return v


liftVO :: ValueOperator -> FieldMapper
liftVO op = SM.gets dynamicContextValue >>= op >>= setValue
