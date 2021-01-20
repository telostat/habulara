module Data.Habulara.Core.Operations where

import           Control.Applicative            (Alternative((<|>)))
import           Control.Monad.Except           (MonadError(throwError))
import           Control.Monad.Reader           (MonadReader(ask))
import           Control.Monad.State            (MonadState(get))
import           Data.Habulara.Core.Types.Class (OperatorM)
import           Data.Habulara.Core.Types.Value (Value(..))
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T


select :: OperatorM m => T.Text -> m Value
select s = maybe (throwError $ "Can not find input field with name: " <> T.unpack s) pure . HM.lookup s =<< ask


select' :: OperatorM m => T.Text -> m Value
select' s = select s <|> pure VEmpty


use :: OperatorM m => T.Text -> m Value
use s = maybe (throwError $ "Can not find buffer field with name: " <> T.unpack s) pure . HM.lookup s =<< get
