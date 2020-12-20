{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Habulara.Internal.Operators.Decimal where

import Control.Monad.Except (MonadError(throwError))
import Data.Habulara.Types
import Data.Scientific      (Scientific)


add :: Scientific -> ValueOperator
add addend (VDecimal x) = pure $ VDecimal (x + addend)
add _ _                 = throwError "Operator can only be applied to values of Decimal type."


sub :: Scientific -> ValueOperator
sub subtrahend (VDecimal x) = pure $ VDecimal (x - subtrahend)
sub _ _                     = throwError "Operator can only be applied to values of Decimal type."


subFrom :: Scientific -> ValueOperator
subFrom minuend (VDecimal x) = pure $ VDecimal (minuend - x)
subFrom _ _                  = throwError "Operator can only be applied to values of Decimal type."


multiply :: Scientific -> ValueOperator
multiply factor (VDecimal x) = pure $ VDecimal (x * factor)
multiply _ _                 = throwError "Operator can only be applied to values of Decimal type."


divide :: Scientific -> ValueOperator
divide dividend (VDecimal x) = pure $ VDecimal (dividend / x)  -- TODO: Encode the possibility of division errors.
divide _ _                   = throwError "Operator can only be applied to values of Decimal type."


divideBy :: Scientific -> ValueOperator
divideBy divisor (VDecimal x) = pure $ VDecimal (x / divisor)  -- TODO: Encode the possibility of division errors.
divideBy _ _                  = throwError "Operator can only be applied to values of Decimal type."


percentage :: ValueOperator
percentage = multiply 100
