{-# LANGUAGE LambdaCase #-}
module Data.Habulara.Inspect.Internal where

import           Control.Monad                   (mzero)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import           Data.Csv                        ((.:))
import qualified Data.Csv                        as Csv
import           Data.Habulara.Dsl.Specification (FieldSpec(..), MappingSpec(..))
import qualified Data.HashMap.Strict             as HM
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Data.Yaml                       as YAML
import           System.Exit                     (ExitCode(..))
import qualified System.Process.Typed            as P


inspect :: MonadIO m => FilePath -> m ExitCode
inspect fp = runXsv fp >>= \case
  Left err -> liftIO (putStrLn err) >> pure (ExitFailure 1)
  Right st -> do
    let estat = fmap (V.toList . snd) (Csv.decodeByName st) :: Either String [Stat]
    case estat of
      Left err -> liftIO (putStrLn err) >> pure (ExitFailure 2)
      Right ss -> liftIO (BC.putStr $ YAML.encode $ prepareSpec ss) >> pure ExitSuccess


prepareSpec :: [Stat] -> Aeson.Value
prepareSpec ss = Aeson.Object $ HM.fromList
  [ ("name", Aeson.String "<NAME>")
  , ("description", Aeson.String "<DESCRIPTION>")
  , ("delimiter", Aeson.String ",")
  , ("encoding", Aeson.String "utf-8")
  , ("fields", Aeson.Array $ V.fromList $ mkFields ss)
  ]
  where
    mkFields :: [Stat] -> [Aeson.Value]
    mkFields [] = []
    mkFields (x : xs) = Aeson.Object (HM.fromList
      [ ("label", Aeson.String $ statField x)
      , ("title", Aeson.String $ statField x)
      , ("description", Aeson.String $ T.pack $ mkDescription x)
      , ("operation", Aeson.Array $ V.fromList $ mkOps x)
      ]) : mkFields xs

    mkOps x = case statType x of
      StatFieldTypeFloat   -> [select, Aeson.Object $ HM.fromList [("name", "asNumber")]]
      StatFieldTypeInteger -> [select, Aeson.Object $ HM.fromList [("name", "asNumber")]]
      StatFieldTypeUnicode -> [select, Aeson.Object $ HM.fromList [("name", "asText")]]
      StatFieldTypeNULL    -> [select, Aeson.Object $ HM.fromList [("name", "asEmpty")]]
      where
        select = Aeson.Object $ HM.fromList [("name", Aeson.String "select"), ("args", Aeson.Array $ V.fromList [Aeson.String $ statField x])]

    mkDescription x =
         "- Name:         " <> T.unpack (statField x) <> "\n"
      <> "- Type:         " <> show (statType x) <> "\n"
      <> "- Min:          " <> fromMaybe "#N/A" (statMin x) <> "\n"
      <> "- Max:          " <> fromMaybe "#N/A" (statMax x) <> "\n"
      <> "- Min Length:   " <> fromMaybe "#N/A" (statMinLength x) <> "\n"
      <> "- Max Length:   " <> fromMaybe "#N/A" (statMaxLength x) <> "\n"
      <> "- Stddev:       " <> fromMaybe "#N/A" (statStddev x) <> "\n"
      <> "- Mean:         " <> fromMaybe "#N/A" (statMean x) <> "\n"
      <> "- Median:       " <> fromMaybe "#N/A" (statMedian x) <> "\n"
      <> "- Cardinality:  " <> fromMaybe "#N/A" (statCardinality x) <> "\n"


runXsv :: MonadIO m => FilePath -> m (Either String BL.ByteString)
runXsv fp = do
  (ec, out, err) <- P.readProcess $ P.proc "xsv" ["stats", "--median", "--cardinality", fp]
  case ec of
    ExitSuccess   -> pure $ Right out
    ExitFailure c -> pure . Left $ "ERROR! `xsv` process exited with code " <> show c <> ". Message was: " <> BLC.unpack err


data Stat = Stat
  { statField       :: !T.Text
  , statType        :: !StatFieldType
  , statMin         :: !(Maybe String)
  , statMax         :: !(Maybe String)
  , statMinLength   :: !(Maybe String)
  , statMaxLength   :: !(Maybe String)
  , statMean        :: !(Maybe String)
  , statStddev      :: !(Maybe String)
  , statMedian      :: !(Maybe String)
  , statCardinality :: !(Maybe String)
  } deriving Show


instance Csv.FromNamedRecord Stat where
  parseNamedRecord m = Stat
    <$> m .: "field"
    <*> m .: "type"
    <*> m .: "min"
    <*> m .: "max"
    <*> m .: "min_length"
    <*> m .: "max_length"
    <*> m .: "mean"
    <*> m .: "stddev"
    <*> m .: "median"
    <*> m .: "cardinality"


data StatFieldType =
    StatFieldTypeFloat
  | StatFieldTypeInteger
  | StatFieldTypeUnicode
  | StatFieldTypeNULL
  deriving Show


instance Csv.FromField StatFieldType where
  parseField s
    | s == "Float"   = pure StatFieldTypeFloat
    | s == "Integer" = pure StatFieldTypeInteger
    | s == "Unicode" = pure StatFieldTypeUnicode
    | s == "NULL"    = pure StatFieldTypeNULL
    | otherwise = mzero
