{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Internal.Mapping where

import           Control.Monad.Identity (Identity(runIdentity))
import           Data.Habulara.Types
                 ( DynamicContext(..)
                 , Field
                 , FieldMapper
                 , FieldMappers
                 , Record
                 , StaticContext(..)
                 , Value(..)
                 , runREST
                 )
import qualified Data.HashMap.Strict    as HM


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
