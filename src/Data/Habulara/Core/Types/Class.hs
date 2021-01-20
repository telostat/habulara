{-# LANGUAGE FlexibleContexts #-}

module Data.Habulara.Core.Types.Class where

import           Control.Applicative             (Alternative)
import           Control.Monad.Except            (MonadError(catchError, throwError))
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader)
import           Control.Monad.State             (MonadState)
import           Data.Habulara.Core.Types.Error  (HabularaError)
import           Data.Habulara.Core.Types.Record (Record)
import qualified Data.Text                       as T


class ( Functor m
      , Applicative m
      , Alternative m
      , Monad m
      , MonadIO m
      , MonadReader Record m
      , MonadState Record m
      , MonadError HabularaError m
      ) => OperatorM m where

  empty :: m a
  empty = throwError "empty"

  (<|>) :: m a -> m a -> m a
  x <|> y = x `catchError` const y

  -- | Logs a debug message.
  debug :: T.Text -> m ()
