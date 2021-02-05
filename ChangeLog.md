# Changelog for Habulara

## 0.0.0.2 (2021-02-05)

Refactoring (or rewriting to be precise) the prototype.

Essentially:

- Entire implementation is based on a custom monad that spans across
  the library and application.
- Processing is done in a streaming-fashion, based on [conduit
  library](https://hackage.haskell.org/package/conduit).
- CLI application is now based on sub-commands.

There are some functionality removed temporarily which will be added
later (such as some field value operators, file encoding handler,
etc).

Also, we have significant performance regression due to the new
underlying code architecture. This is not much worrying as
expressiveness and control over the program execution is much more
important at the moment.

### Features

* **app:** start proper CLI user experience, add "process" command ([2834a59](https://github.com/telostat/habulara/commit/2834a59a8f5e1ab4d137e53dacb371902be5908c))

## 0.0.0.1 (2020-12-22)

### Features

* add code generator for Haskell `data` definitions ([8e6e3d6](https://github.com/telostat/habulara/commit/8e6e3d65325f6154d2f19cacc8d64a0e927ca89f))
* add initial DSL implementation for mapping ([bb5c56f](https://github.com/telostat/habulara/commit/bb5c56f97ac56e167f3eb4e38249dd4a61468439))
