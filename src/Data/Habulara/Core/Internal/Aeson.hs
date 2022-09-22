module Data.Habulara.Core.Internal.Aeson where

import qualified Data.Aeson as Aeson
import Data.Habulara.Dsl.Operators (lowerFirst)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)


-- | Common Aeson encoding/decoding options.
commonAesonOptions :: String -> Aeson.Options
commonAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.fieldLabelModifier = \l -> lowerFirst . fromMaybe l $ stripPrefix prefix l
    , Aeson.constructorTagModifier = \l -> lowerFirst . fromMaybe l $ stripPrefix prefix l
    , Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type"
          , Aeson.contentsFieldName = "value"
          }
    }
