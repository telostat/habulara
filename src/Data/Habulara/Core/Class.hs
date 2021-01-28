{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Habulara.Core.Class where

import           Control.Applicative    (Alternative(empty, (<|>)))
import           Control.Monad.Except   (ExceptT, MonadError(catchError, throwError), MonadPlus(..), runExceptT)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader   (MonadReader, ReaderT(ReaderT, runReaderT))
import           Control.Monad.State    (MonadState, StateT(runStateT))
import qualified Data.Text              as T
import           System.IO              (hPutStrLn, stderr)


-- | Habulara error type.
--
-- __Note[vst]:__ Do we want to make the 'HabularaError' extensible? This will
-- introduce some complexity to 'MonadHabulara' which we may wish to avoid.
data HabularaError =
    HabularaErrorEmpty
  | HabularaErrorCsv String
  | HabularaErrorOperation T.Text
  | HabularaErrorRead String
  | HabularaErrorSimple T.Text
  | HabularaErrorValueConversion String
  deriving Show


-- | Core 'Monad' for Habulara programs.
class ( Functor m
      , Applicative m
      , Alternative m
      , Monad m
      , MonadIO m
      , MonadReader r m
      , MonadState s m
      , MonadError HabularaError m
      ) => MonadHabulara r s m | m -> r s where

  -- | Logs a debug message.
  debug :: T.Text -> m ()


-- | Habulara monad based on a transformer stack.
--
-- @TODO:@ Revisit instance declarations.
newtype HabularaT r s m a = HabularaT { unHabularaT :: ReaderT r (StateT s (ExceptT HabularaError m)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader r
    , MonadState s
    , MonadError HabularaError
    )

instance (Monad m) => Alternative (HabularaT r s m) where
  empty = mzero
  (<|>) = mplus

instance (Monad m) => MonadPlus (HabularaT r s m) where
  mzero = throwError HabularaErrorEmpty
  m0 `mplus` m1 = m0 `catchError` const m1  -- @TODO:@ Is it lawful in the first place?

instance (MonadIO m) => (MonadHabulara r s) (HabularaT r s m) where
  debug = liftIO . hPutStrLn stderr . T.unpack


-- | Runs a 'HabularaT' in the given environement @r@ with the given initial
-- state @s@.
--
-- >>> import Control.Monad.Reader (asks)
-- >>> import Control.Monad.State (modify, get)
-- >>> runHabularaT () () (debug $ T.pack "Hebele") :: IO (Either HabularaError ((), ()))
-- Hebele
-- Right ((),())
-- >>> runHabularaT 41 () (asks ((+) 1)) :: IO (Either HabularaError (Int, ()))
-- Right (42,())
-- >>> runHabularaT () 41 (modify ((+) 1)) :: IO (Either HabularaError ((), Int))
-- Right ((),42)
-- >>> runHabularaT 41 1 (get >>= asks . (+)) :: IO (Either HabularaError (Int, Int))
-- Right (42,1)
-- >>> runHabularaT 0 42 (throwError HabularaErrorEmpty <|> get) :: IO (Either HabularaError (Int, Int))
-- Right (42,42)
runHabularaT
  :: r
  -> s
  -> HabularaT r s m a
  -> m (Either HabularaError (a, s))
runHabularaT env state program = runExceptT (runStateT (runReaderT (unHabularaT program) env) state)


-- | Re-declaration of 'HabularaT' execution ('runHabularaT') in 'IO' monad for convenience
-- purposes such as doctests without being forced to give type hints.
runHabularaIO
  :: r
  -> s
  -> HabularaT r s IO a
  -> IO (Either HabularaError (a, s))
runHabularaIO = runHabularaT


-- | Run a 'HabularaT' execution ('runHabularaT') in 'IO' monad with unit
-- environment and state variables for convenience purposes such as doctests
-- without being forced to give type hints.
runHabularaInVoid
  :: HabularaT () () IO a
  -> IO (Either HabularaError (a, ()))
runHabularaInVoid = runHabularaIO () ()
