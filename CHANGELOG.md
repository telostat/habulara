# CHANGELOG

All notable changes to this project will be documented in this file.
This project adheres to [Haskell Package Versioning
Policy](https://pvp.haskell.org/).

<a name="unreleased"></a>
## [Unreleased]


<a name="0.0.0.4"></a>
## [0.0.0.4] - 2022-09-22
### Features
- **inspect:** add inspect command that produces a YAML specification

### Code Refactoring
- drop TemplateHaskell usage for Aeson.FromJSON instances
- adopt custom hlint rules, attend hlint warnings
- adapt to Aeson v2
- **style:** adopt fourmolu (v0.8.2)


<a name="0.0.0.3"></a>
## [0.0.0.3] - 2021-02-09
### Features
- accept non-UTF8 CSV input text data
- **operators:** add new conditional operators


<a name="0.0.0.2"></a>
## [0.0.0.2] - 2021-02-05
### Features
- enable UTF-8 encoding if the input file is not encoded as such
- add Date/Time type
- **app:** start proper CLI user experience, add "process" command

### Bug Fixes
- lower the tail of text while capitalizing

### Code Refactoring
- update README
- review and reorganize examples, benchmarks
- review Mapping module, change and adopt some definitions
- re-implement DSL, make application work again
- make field accessors work with Value arguments
- re-organize Mapping and Operation modules, reformat
- purge old implementation
- add date/time operators
- add boolean operations
- add textual operators
- add numeric operators
- add type-guarded value operators
- change final 'Value' constructor name, adopt in operations
- rename 'VBoolean' constructor to 'VBool'
- rename 'VDecimal' constructor to 'VNumber'
- drop Integer based 'VInt' value type
- re-organize types under Data.Habulara.Core.Types
- drop ByteString based 'VRaw' value type
- improve value conversion semantics
- add 'liftMaybe' to lift 'Maybe' into 'MonadHabulara'
- update stack.yaml, regenerate .cabal file
- start with tests (QuickCheck), ditch Examples, reorganize
- revisit Operations module
- split Types modules, improve Value and Operations
- add another convenience for running HabularaT inside IO
- add OverloadedString extension by default
- add convenience function for running HabularaT inside IO
- implement 'NonEmpty' type
- update sample data generator and add benchmark script
- fix space leak due to laziness in incrementing row counter
- return total number of rows processed from mapper
- reorganize core library, start using Conduit
- introduce VRaw (ByteString-based Value type)
- continue refactoring
- start refactoring


<a name="0.0.0.1"></a>
## 0.0.0.1 - 2020-12-22
### Features
- add code generator for Haskell `data` definitions
- add initial DSL implementation for mapping


[Unreleased]: https://github.com/telostat/habulara/compare/0.0.0.4...HEAD
[0.0.0.4]: https://github.com/telostat/habulara/compare/0.0.0.3...0.0.0.4
[0.0.0.3]: https://github.com/telostat/habulara/compare/0.0.0.2...0.0.0.3
[0.0.0.2]: https://github.com/telostat/habulara/compare/0.0.0.1...0.0.0.2
