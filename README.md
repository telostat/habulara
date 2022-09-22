# Habulara - Toolkit for Processing Tabular Data

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/habulara)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/habulara)
![GitHub](https://img.shields.io/github/license/telostat/habulara)

> **Note:** This package is under heavy development and of prototype
> quality at the moment. Expect significant performance regressions
> across releases and breaking changes without notification until we
> reach the first major version.

Habulara is a Haskell library and command line application that
provides high-level means to process tabular data, in particular CSV
files in a declarative fashion.

## Installation

If you are on Nix:

```sh
git clone git@github.com:telostat/habulara.git
cd habulara
nix-env -i -f default.nix
```

Alternatively, you can check statically built binaries under
[releases](https://github.com/telostat/habulara/releases).

## Usage

### As a standalone application

To process a CSV file as per a given HAB (Habulara mapping
specification) file:

```
$ habulara process --help
Usage: habulara process --spec SPEC [--input INPUT] [--output OUTPUT]
  Process given CSV data with given specification

Available options:
  --spec SPEC              Habulara mapper specification filepath
  --input INPUT            Input CSV data filepath (`-` for stdin, default)
  --output OUTPUT          Output CSV data filepath (`-` for stdout, default)
  -h,--help                Show this help text
```

... example:

```
habulara process --spec var/examples/weather-stations/spec.yaml --input var/examples/weather-stations/data.csv
```

### As a library

> **TODO:** Add a quick example for demonstrating library usage.

## License

Copyright Telostat Pte Ltd (c) 2020-2022.

This work is licensed under BSD3. Please check the license file
included in the source-code.

## Development

### Releasing

Below is the release process. Run these under the `nix-shell`, but `nix-shell
--pure` won't work for now due to missing `nix-build` command (We will attend it
later).

Note that `<NEW-VERSION>` is given without `v` prefix (For example: `1.0.0`).

```sh
git checkout main
git pull
./release.sh -n <NEW-VERSION>
```
