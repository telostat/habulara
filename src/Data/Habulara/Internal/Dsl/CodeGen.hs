{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Habulara.Internal.Dsl.CodeGen where

import           Data.Habulara.Internal.Commons.String (lowerFirst, upperFirst)
import           Data.Habulara.Internal.Dsl.Types      (As(..), FieldSpec(..), FileSpec(..))
import           Data.Maybe                            (fromMaybe)
import           Data.String.Interpolate               (i)
import qualified Data.Text                             as T


generateData :: FileSpec -> T.Text
generateData fs = [i|data #{name} = Mk#{name}
  { #{fields}
  } deriving (Generic)

instance DefaultOrdered #{name} where
    headerOrder = genericHeaderOrder $ defaultOptions { fieldLabelModifier = lowerFirst . drop #{ncnt} }

instance ToNamedRecord #{name} where
  toNamedRecord = genericToNamedRecord $ defaultOptions { fieldLabelModifier = lowerFirst . drop #{ncnt} }

instance FromNamedRecord #{name} where
  parseNamedRecord = genericParseNamedRecord $ defaultOptions { fieldLabelModifier = lowerFirst . drop #{ncnt} }
|]
  where
    name = fromMaybe (fileSpecName fs) (fileSpecDataName fs)
    ncnt = T.length name
    fields = T.intercalate "\n  , " $ fmap (generateDataField name maxLen1 maxLen2) (fileSpecFields fs)
    maxLen1 = maximum $ fmap (T.length . fieldSpecTargetColumn) (fileSpecFields fs)
    maxLen2 = maximum $ fmap (T.length . toHaskellType . fieldSpecType) (fileSpecFields fs)


generateDataField :: T.Text -> Int -> Int -> FieldSpec -> T.Text
generateDataField tname padlen1 padlen2 fs = T.strip [i|#{prefix}#{name}#{padding1} :: !#{ftype}#{padding2}#{help}|]
  where
    prefix = lowerFirst $ T.unpack tname
    name = upperFirst $ T.unpack $ fieldSpecTargetColumn fs
    ftype = toHaskellType $ fieldSpecType fs
    padding1 = T.replicate (padlen1 - length name) " "
    padding2 = T.replicate (padlen2 - T.length ftype) " "
    help = maybe "" ("  -- ^ " <> ) (fieldSpecShortDescription fs)


toHaskellType :: As -> T.Text
toHaskellType AsEmpty   = "()"
toHaskellType AsRaw     = "ByteString"
toHaskellType AsInt     = "Integer"
toHaskellType AsText    = "Text"
toHaskellType AsDecimal = "Scientific"
toHaskellType AsBoolean = "Bool"
toHaskellType AsDate    = "Day"
