{-# LANGUAGE LambdaCase #-}

module Data.Habulara.Internal.Combinators.Primitives where

import           Control.Monad.Except                       (MonadError(throwError))
import qualified Control.Monad.Except                       as EM
import           Control.Monad.Reader                       (asks)
import qualified Control.Monad.State                        as SM
import qualified Data.ByteString                            as B
import           Data.Habulara.Internal.Combinators.Commons (Operator, setField, setFieldValue, setValue)
import           Data.Habulara.Types                        (DynamicContext(..), StaticContext(..), Value(..))
import qualified Data.HashMap.Strict                        as HM
import           Data.Scientific                            (Scientific)
import qualified Data.Text                                  as T
import           Data.Time                                  (Day)


-- ** Primitives
--
-- $primitives


select :: T.Text -> Operator
select f = asks (HM.lookup f . staticContextRecord) >>= \case
  Nothing -> EM.throwError $ "No such field found in the raw record: " <> T.unpack f
  Just fv -> setFieldValue f fv


use :: T.Text -> Operator
use f = SM.gets (HM.lookup f . dynamicContextBuffer) >>= \case
  Nothing -> EM.throwError $ "No such field found in the buffer record: " <> T.unpack f
  Just fv -> setFieldValue f fv


useAs :: T.Text -> T.Text -> Operator
useAs f n = SM.gets (HM.lookup f . dynamicContextBuffer) >>= \case
  Nothing -> EM.throwError $ "No such field found in the buffer record: " <> T.unpack f
  Just fv -> setFieldValue n fv


rename :: T.Text -> Operator
rename = setField


empty :: Operator
empty = setValue VEmpty


error :: T.Text -> Operator
error = throwError . T.unpack


raw :: B.ByteString -> Operator
raw = setValue . VRaw


int :: Integer -> Operator
int = setValue . VInt


text :: T.Text -> Operator
text = setValue . VText


decimal :: Scientific -> Operator
decimal = setValue . VDecimal


boolean :: Bool -> Operator
boolean = setValue . VBoolean


true :: Operator
true = boolean True


false :: Operator
false = boolean False


date :: Day -> Operator
date = setValue . VDate
