{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Internal.Combinators.Numeric where

import           Control.Monad.Except                       (MonadError(throwError))
import qualified Control.Monad.State                        as SM
import qualified Data.ByteString.Char8                      as BC
import           Data.Habulara.Internal.Combinators.Commons (Operator, setValue)
import           Data.Habulara.Types                        (DynamicContext(..), Value(..))
import           Data.Scientific                            (Scientific, toBoundedInteger)
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as TE
import           Data.Time                                  (Day(toModifiedJulianDay))
import           Text.Read                                  (readMaybe)


-- ** Numeric
--
-- $numeric

asInt :: Operator
asInt = SM.gets dynamicContextValue >>= ensureVInt >>= setValue


asDecimal :: Operator
asDecimal = SM.gets dynamicContextValue >>= ensureVDecimal >>= setValue


asPercentage :: Operator
asPercentage = SM.gets dynamicContextValue >>= ensureVDecimal >>= aux >>= setValue
  where
    aux (VDecimal d) = pure $ VDecimal $ d * 100
    aux _            = throwError "'asPercentage' can only be applied to values of decimal type"


multiply :: Scientific -> Operator
multiply x = SM.gets dynamicContextValue >>= ensureVDecimal >>= aux >>= setValue
  where
    aux (VDecimal d) = pure $ VDecimal $ d * x
    aux _            = throwError "'multiply' can only be applied to value of decimal type"


divideBy :: Scientific -> Operator
divideBy x = SM.gets dynamicContextValue >>= ensureVDecimal >>= aux >>= setValue
  where
    aux (VDecimal d) = pure $ VDecimal $ d / x  -- TODO: Encode the possibility of division errors.
    aux _            = throwError "'divideBy' can only be applied to value of decimal type"


add :: Scientific -> Operator
add x = SM.gets dynamicContextValue >>= ensureVDecimal >>= aux >>= setValue
  where
    aux (VDecimal d) = pure $ VDecimal $ d + x
    aux _            = throwError "'add' can only be applied to value of decimal type"


--------------
-- INTERNAL --
--------------

ensureVInt :: MonadError String m => Value -> m Value
ensureVInt VEmpty = pure VEmpty
ensureVInt (VRaw b) = case readMaybe (BC.unpack b) of
  Nothing -> throwError $ show $ "Value can not be converted to Integer: " <> TE.decodeUtf8 b
  Just sv -> pure $ VInt sv
ensureVInt (VInt i) = pure $ VInt i
ensureVInt (VText b) = case readMaybe (T.unpack b) of
  Nothing -> throwError $ show $ "Value can not be converted to Integer: " <> b
  Just sv -> pure $ VInt sv
ensureVInt (VDecimal s) = case toBoundedInteger s of
  Nothing -> throwError $ show $ "Value can not be converted to Integer: " <> T.pack (show s)
  Just sv -> pure $ VInt $ toInteger (sv :: Int)
ensureVInt (VBoolean False) = pure $ VInt 0
ensureVInt (VBoolean True) = pure $ VInt 1
ensureVInt (VDate d) = pure $ VInt $ toModifiedJulianDay d


ensureVDecimal :: MonadError String m => Value -> m Value
ensureVDecimal VEmpty = pure VEmpty
ensureVDecimal (VRaw b) = case readMaybe (BC.unpack b) of
  Nothing -> throwError $ show $ "Value can not be converted to Decimal: " <> TE.decodeUtf8 b
  Just sv -> pure $ VDecimal sv
ensureVDecimal (VInt i) = pure $ VDecimal $ fromInteger i
ensureVDecimal (VText b) = case readMaybe (T.unpack b) of
  Nothing -> throwError $ show $ "Value can not be converted to Decimal: " <> b
  Just sv -> pure $ VDecimal sv
ensureVDecimal (VDecimal s) = pure $ VDecimal s
ensureVDecimal (VBoolean False) = pure $ VDecimal 0
ensureVDecimal (VBoolean True) = pure $ VDecimal 1
ensureVDecimal (VDate d) = pure $ VDecimal . fromInteger . toModifiedJulianDay $ d
