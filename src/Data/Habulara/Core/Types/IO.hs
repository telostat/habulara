{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Habulara.Core.Types.IO where

import           Control.Applicative             (Alternative)
import           Control.Monad.Except            (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Control.Monad.Reader            (MonadReader, ReaderT(..))
import           Control.Monad.State             (MonadState, StateT(..))
import           Data.Habulara.Core.Types.Class  (OperatorM(..))
import           Data.Habulara.Core.Types.Error  (HabularaError)
import           Data.Habulara.Core.Types.Record (Record)
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import           System.IO                       (hPutStrLn, stderr)


newtype OperatorIO a = OperatorIO {unOperatorIO :: ReaderT Record (ExceptT HabularaError (StateT Record IO)) a}
  deriving
    ( MonadIO
    , Functor
    , Applicative
    , Alternative
    , Monad
    , MonadReader Record
    , MonadState Record
    , MonadError HabularaError
    )


instance OperatorM OperatorIO where
  debug = liftIO . hPutStrLn stderr . T.unpack


runOperatorIO
  :: Record        -- ^ Raw 'Record'.
  -> OperatorIO a  -- ^ 'OperatorIO' to run.
  -> IO (Either HabularaError a, Record)
runOperatorIO env program = runStateT (runExceptT (runReaderT (unOperatorIO program) env)) HM.empty
