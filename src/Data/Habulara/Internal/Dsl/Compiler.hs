module Data.Habulara.Internal.Dsl.Compiler where

import           Control.Monad                    ((>=>))
import qualified Data.ByteString                  as B
import           Data.Habulara.Internal.Dsl.Types (FieldSpec(..), FileSpec(..), opToOperator)
import           Data.Habulara.Internal.Mapping   (selectAs, selectAsOrEmpty, (~>))
import           Data.Habulara.Types              (FieldMapper)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text.Encoding               as TE


fileSpecToHeader :: FileSpec -> [B.ByteString]
fileSpecToHeader = fmap (TE.encodeUtf8 . fieldSpecTargetColumn) . fileSpecFields


fieldSpecToMapper :: FieldSpec -> FieldMapper
fieldSpecToMapper fs = selector sourceColumn targetColumn ~> foldl (\x y -> x >=> opToOperator y) pure operators
  where
    targetColumn = fieldSpecTargetColumn fs
    sourceColumn = fromMaybe targetColumn $ fieldSpecSourceColumn fs
    operators = fieldSpecAction fs
    selector = if fieldSpecAllowMissingColumn fs then selectAsOrEmpty else selectAs
