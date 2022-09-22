{-# LANGUAGE DeriveGeneric #-}

module Data.Habulara.Dsl.Specification where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Habulara (HabularaError (..), Label)
import Data.Habulara.Core.Conduit (runMapperIntoHandle)
import Data.Habulara.Core.Internal.Aeson (commonAesonOptions)
import Data.Habulara.Dsl.Operators (Op, compileOperator)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.IO (Handle)


data FieldSpec = FieldSpec
  { fieldSpecLabel :: !Label
  , fieldSpecTitle :: !(Maybe T.Text)
  , fieldSpecDescription :: !(Maybe T.Text)
  , fieldSpecOperation :: !(Maybe (NE.NonEmpty Op))
  }
  deriving (Generic, Show)


instance Aeson.FromJSON FieldSpec where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "fieldSpec"


data MappingSpec = MappingSpec
  { mappingSpecName :: !T.Text
  , mappingSpecDescription :: !(Maybe T.Text)
  , mappingSpecDelimiter :: !(Maybe Char)
  , mappingSpecEncoding :: !(Maybe String)
  , mappingSpecFields :: !(NE.NonEmpty FieldSpec)
  }
  deriving (Generic, Show)


instance Aeson.FromJSON MappingSpec where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "mappingSpec"


readSpec :: BL.ByteString -> Either String MappingSpec
readSpec = either (Left . show) Right . Yaml.decodeEither' . BL.toStrict


readSpecHandle :: MonadIO io => Handle -> io (Either String MappingSpec)
readSpecHandle h = readSpec <$> liftIO (BL.hGetContents h)


readSpecFilePath :: MonadIO io => FilePath -> io (Either String MappingSpec)
readSpecFilePath filepath = readSpec <$> liftIO (BL.readFile filepath)


runWithSpecIntoHandle :: MonadIO io => MappingSpec -> BL.ByteString -> Handle -> io (Either HabularaError ((), Integer))
runWithSpecIntoHandle ms = runMapperIntoHandle delimiter encoding fops True
  where
    encoding = mappingSpecEncoding ms
    compiler (FieldSpec l _ _ o) = compileOperator l (foldMap NE.toList o)
    delimiter = fromMaybe ',' (mappingSpecDelimiter ms)
    fops = fmap (fieldSpecLabel &&& compiler) (NE.toList $ mappingSpecFields ms)


runIntoHandle :: MonadIO io => BL.ByteString -> BL.ByteString -> Handle -> io (Either HabularaError ((), Integer))
runIntoHandle specContent dataContent handle = do
  let eSpec = readSpec specContent
  case eSpec of
    Left err -> pure . Left . HabularaErrorSimple $ T.pack err
    Right spec -> runWithSpecIntoHandle spec dataContent handle
