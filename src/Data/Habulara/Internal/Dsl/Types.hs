{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Dsl.Types where

import           Data.Aeson
                 ( FromJSON(..)
                 , Options(..)
                 , Value(..)
                 , defaultOptions
                 , genericParseJSON
                 )
import           Data.Habulara.Internal.Commons.String (lowerFirst)
import qualified Data.Habulara.Internal.Operators      as O
import           Data.Habulara.Types                   (ValueOperator)
import           Data.Scientific                       (Scientific)
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Data.Vector                           as V
import           GHC.Generics                          (Generic)


data FileSpec = FileSpec
  { fileSpecName       :: !T.Text
  , fileSpecDataName   :: !(Maybe T.Text)
  , fileSpecDecription :: !(Maybe T.Text)
  , fileSpecDelimiter  :: !Char
  , fileSpecFields     :: ![FieldSpec]
  } deriving (Generic, Show)

instance FromJSON FileSpec where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }


defaultFileSpec :: FileSpec
defaultFileSpec = FileSpec "Records" Nothing Nothing ',' []


data FieldSpec = FieldSpec
  { fieldSpecType               :: !As
  , fieldSpecTargetColumn       :: !T.Text
  , fieldSpecSourceColumn       :: !(Maybe T.Text)
  , fieldSpecShortDescription   :: !(Maybe T.Text)
  , fieldSpecLongDescription    :: !(Maybe T.Text)
  , fieldSpecAllowMissingColumn :: !Bool
  , fieldSpecAllowEmptyValue    :: !Bool
  , fieldSpecAction             :: ![Op]
  } deriving (Generic, Show)

instance FromJSON FieldSpec where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = lowerFirst . drop 9}


defaultFieldSpec :: [Op] -> FieldSpec
defaultFieldSpec = FieldSpec AsRaw "column" Nothing Nothing Nothing True True


data As =
    AsEmpty
  | AsRaw
  | AsInt
  | AsText
  | AsDecimal
  | AsBoolean
  | AsDate
  deriving (Show)

instance FromJSON As where
  parseJSON (String x) = asFromString $ T.unpack x
  parseJSON _          = fail "Invalid value type for 'As' value"


asFromString :: MonadFail m => String -> m As
asFromString "Empty"   = pure AsEmpty
asFromString "Raw"     = pure AsRaw
asFromString "Int"     = pure AsInt
asFromString "Text"    = pure AsText
asFromString "Decimal" = pure AsDecimal
asFromString "Boolean" = pure AsBoolean
asFromString "Date"    = pure AsDate
asFromString x         = fail $ "Unrecognized 'As' value: " <> x


data Op =
    OpEmpty
  | OpRaw
  | OpInt
  | OpText
  | OpDecimal
  | OpBoolean
  | OpDate
  | OpTrim
  | OpSanitize
  | OpLower
  | OpUpper
  | OpCapitalize
  | OpAppend !T.Text
  | OpPrepend !T.Text
  | OpSplitHead !Char
  | OpSplitTail !Char
  | OpOneOfText !(S.Set T.Text)
  | OpConstant !T.Text
  | OpConstantEmpty
  | OpParseDate !String
  | OpBooleanMap !T.Text !T.Text
  | OpBooleanMapCI !T.Text !T.Text
  | OpAdd !Scientific
  | OpSub !Scientific
  | OpSubFrom !Scientific
  | OpMultiply !Scientific
  | OpDivide !Scientific
  | OpDivideBy !Scientific
  | OpPercentage
  deriving (Show)

instance FromJSON Op where
  parseJSON (String "empty") = pure OpEmpty
  parseJSON (String "raw") = pure OpRaw
  parseJSON (String "int") = pure OpInt
  parseJSON (String "text") = pure OpText
  parseJSON (String "decimal") = pure OpDecimal
  parseJSON (String "boolean") = pure OpBoolean
  parseJSON (String "date") = pure OpDate
  parseJSON (String "trim") = pure OpTrim
  parseJSON (String "sanitize") = pure OpSanitize
  parseJSON (String "lower") = pure OpLower
  parseJSON (String "upper") = pure OpUpper
  parseJSON (String "capitalize") = pure OpCapitalize
  parseJSON (String "constantEmpty") = pure OpConstantEmpty
  parseJSON (String "percentage") = pure OpPercentage
  parseJSON (Array xs) = attemptFromArray (V.toList xs)
    where
      attemptFromArray :: MonadFail m => [Value] -> m Op
      attemptFromArray [String "append", String x] = pure $ OpAppend x
      attemptFromArray [String "prepend", String x] = pure $ OpPrepend x
      attemptFromArray [String "splitHead", String x] = pure $ OpSplitHead $ T.head x  -- TODO: handle empty
      attemptFromArray [String "splitTail", String x] = pure $ OpSplitTail $ T.head x  -- TODO: handle empty
      attemptFromArray [String "oneOfText", String x] = pure $ OpOneOfText $ S.fromList $ T.split (== '|') x  -- TODO: handle empty
      attemptFromArray [String "constant", String x] = pure $ OpConstant x
      attemptFromArray [String "parseDate", String x] = pure $ OpParseDate (T.unpack x)
      attemptFromArray [String "booleanMap", String x, String y] = pure $ OpBooleanMap x y
      attemptFromArray [String "booleanMapCI", String x, String y] = pure $ OpBooleanMapCI x y
      attemptFromArray [String "add", Number x] = pure $ OpAdd x
      attemptFromArray [String "sub", Number x] = pure $ OpSub x
      attemptFromArray [String "subFrom", Number x] = pure $ OpSubFrom x
      attemptFromArray [String "multiply", Number x] = pure $ OpMultiply x
      attemptFromArray [String "divide", Number x] = pure $ OpDivide x
      attemptFromArray [String "divideBy", Number x] = pure $ OpDivideBy x
      attemptFromArray x = fail $ "Unknown piece of action: " <> show x
  parseJSON _ = pure OpConstantEmpty


opToOperator :: Op -> ValueOperator
opToOperator OpEmpty              = O.vEmpty
opToOperator OpRaw                = O.vRaw
opToOperator OpInt                = O.vInt
opToOperator OpText               = O.vText
opToOperator OpDecimal            = O.vDecimal
opToOperator OpBoolean            = O.vBoolean
opToOperator OpDate               = O.vDate
opToOperator OpTrim               = O.trim
opToOperator OpSanitize           = O.sanitize
opToOperator OpLower              = O.lower
opToOperator OpUpper              = O.upper
opToOperator OpCapitalize         = O.capitalize
opToOperator (OpAppend x)         = O.append x
opToOperator (OpPrepend x)        = O.prepend x
opToOperator (OpSplitHead x)      = O.splitHead x
opToOperator (OpSplitTail x)      = O.splitTail x
opToOperator (OpOneOfText x)      = O.oneOfText x
opToOperator (OpConstant x)       = O.constant x
opToOperator OpConstantEmpty      = O.constantEmpty
opToOperator (OpParseDate x)      = O.parseDate x
opToOperator (OpBooleanMap x y)   = O.booleanMap x y
opToOperator (OpBooleanMapCI x y) = O.booleanMapCI x y
opToOperator (OpAdd x)            = O.add x
opToOperator (OpSub x)            = O.sub x
opToOperator (OpSubFrom x)        = O.subFrom x
opToOperator (OpMultiply x)       = O.multiply x
opToOperator (OpDivide x)         = O.divide x
opToOperator (OpDivideBy x)       = O.divideBy x
opToOperator OpPercentage         = O.percentage
