{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Types where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT(runReaderT))
import           Control.Monad.State    (StateT(runStateT))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import           Data.Scientific        (Scientific)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (Day)
import           Data.Time.LocalTime    (LocalTime)


-- $core
--
-- The basic idea of Habulara is to take a raw CSV row record and map it into a
-- Habulara CSV row record:
--
--
-- > ┌────────────────┐             ┌─────────────────────┐
-- > │ Raw CSV Record │──── map ───>│ Habulara CSV Record │
-- > └────────────────┘             └─────────────────────┘
--
--
-- Essential types are given below.


-- * Essential Types
--
-- $essentialTypes
--
-- Habulara CSV record type is encoded as a 'Record' which is a @'HM.HashMap'
-- 'Field' 'Value'@. These three types are given in this section.

-- | Record type.
type Record = HM.HashMap Field Value


-- | Record field name type.
--
-- To avoid unicode issues, Habulara is encoding field names as 'T.Text' values.
type Field = T.Text


-- | Record field value type.
data Value =
    VEmpty
  | VInt      !Integer
  | VText     !T.Text
  | VDecimal  !Scientific
  | VBoolean  !Bool
  | VDate     !Day
  | VDateTime !LocalTime
  deriving (Show)

instance Csv.ToField Value where
  toField VEmpty           = B.empty
  toField (VInt i)         = BC.pack $ show i
  toField (VText t)        = TE.encodeUtf8 t
  toField (VDecimal d)     = BC.pack $ show d
  toField (VBoolean True)  = "True"
  toField (VBoolean False) = "False"
  toField (VDate d)        = BC.pack $ show d
  toField (VDateTime x)    = BC.pack $ show x

instance Csv.FromField Value where
  -- TODO: Use decodeUtf8' to catch UnicodeException and propagate.
  parseField x
    | B.null x  = pure VEmpty
    | otherwise = pure . VText . TE.decodeUtf8 $ x


-- * Mapping
--
-- $mapping
--
-- Habulara maps a raw CSV record into a Habulara CSV record using a list of
-- 'FieldMapper's. A single field is mapped via 'FieldMapper' under the 'REST'
-- monad stack whereby the environment containts the raw CSV record, the state
-- is the field name ('Field') and the error type is a 'HabularaError'.


-- | Field mappers.
type FieldMappers = [FieldMapper]


-- | Field mapper monad.
type FieldMapper = REST StaticContext HabularaError DynamicContext Identity Value


-- | Value operator type.
type ValueOperator =  forall m . (Monad m) => HabularaErrorM m => Value -> m Value


-- | Habulara error monad type.
type HabularaErrorM m = MonadError HabularaError m


-- | Habulara error type.
--
-- This is an interim definition. We may wish to use a sum type for it.
type HabularaError = String


-- | Field mapper static context (used as the environment in the field mapper monad).
newtype StaticContext = MkStaticContext { staticContextRecord :: Record } deriving (Show)


-- | Field mapper dynamic context (used as the state in the field mapper monad).
data DynamicContext = MkDynamicContext
  { dynamicContextField  :: !Field
  , dynamicContextValue  :: !Value
  , dynamicContextBuffer :: !Record
  }


-- * Underlying Monad
--
-- $underlyingMonad
--
-- Habulara is using a monad stack as the underlying monad to map fields, namely
-- 'REST'.
--
-- In particular, it is a stack of 'ReaderT', 'ExceptT' and 'StateT' monads.
--
-- The concrete monad is 'FieldMapper' which is a type alias to @'REST'
-- 'StaticContext' 'HabularaError' 'DynamicContext' 'Identity' 'Value'@.


-- | Our underlying monad stack, a stack of 'ReaderT', 'ExceptT' and 'StateT'.
type REST r e s m a = ReaderT r (ExceptT e (StateT s m)) a


-- | Runs the 'REST' monad.
--
runREST :: Monad m => s -> r -> REST r e s m a -> m (Either e a, s)
runREST state env app = runStateT (runExceptT (runReaderT app env)) state
