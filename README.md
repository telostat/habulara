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
habulara var/examples/weather-stations/spec.yaml var/examples/weather-stations/data.csv
```

### As a library

> **TODO:** Add a quick example for demonstrating library usage.

## License

Copyright Telostat Pte Ltd (c) 2020-2021.

This work is licensed under BSD3. Please check the license file
included in the source-code.
