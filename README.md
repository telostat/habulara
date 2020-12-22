# Habulara - Toolkit for Processing Tabular Data

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/habulara)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/habulara)
![GitHub](https://img.shields.io/github/license/telostat/habulara)

> **Note:** This package is under heavy development. Expect breaking
> changes without notification until we reach the first major version.

Habulara is a Haskell library and command line application which
provide high-level means to process tabular data, in particular CSV
files.

## Installation

```
stack install habulara
```

## Usage

### As a standalone application

To process a CSV file as per a given HAB (Habulara mapping
specification) file:

```
habulara test/examples/simple.yaml test/examples/simple.csv
```

To generate Haskell `data` definition:

```
habulara test/examples/simple.yaml
```

### As a library

```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Habulara.Dsl
import           System.IO


main :: IO ()
main = do
  content <- BL.readFile "test/examples/simple.csv"
  case process fileSpec content of
    Left err -> hPutStrLn stderr $ "Error while processing the file: " <> err
    Right oc -> BLC.hPutStr stdout oc


fileSpec :: FileSpec
fileSpec = FileSpec "Latest Weather Figures" Nothing Nothing ','
  [ FieldSpec AsText "id" Nothing (Just "ID") (Just "ID of the record") False False []
  , FieldSpec AsText "name" Nothing (Just "Name") (Just "Name of the record") False False []
  , FieldSpec AsDecimal "tempCelcius" (Just "temperature") (Just "Temperature (C)") (Just "Temperature in Celcius") False False [OpDecimal]
  , FieldSpec AsDecimal "tempFahrenheit" (Just "temperature") (Just "Temperature (F)") (Just "Temperature in Fahrenheit") False False [OpDecimal, OpMultiply 9, OpDivideBy 5, OpAdd 32]
  , FieldSpec AsDecimal "precipitation" Nothing (Just "Precipitation (%)") (Just "Precipitation in percentage points") False False [OpDecimal, OpPercentage]
  ]
```

## License

Copyright Telostat Pte Ltd (c) 2020.

This work is licensed under BSD3. Please check the license file
included in the source-code.
