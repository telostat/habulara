module Data.Habulara.Internal.Combinators.Commons where

import qualified Control.Monad.State as SM
import           Data.Habulara.Types (DynamicContext(..), Field, FieldMapper, Value(..))


type Operator = FieldMapper


setField :: Field -> Operator
setField f = SM.modify (\dc -> dc {dynamicContextField = f}) >> SM.gets dynamicContextValue


setValue :: Value -> Operator
setValue v = SM.modify (\dc -> dc {dynamicContextValue = v}) >> return v


setFieldValue :: Field -> Value -> Operator
setFieldValue f v = SM.modify (\dc -> dc {dynamicContextField = f, dynamicContextValue = v}) >> return v
