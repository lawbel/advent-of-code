# Advent Of Code 2021

Doing the Advent of Code 2021 challenges in Haskell.

## Cabal Structure

We expose all the code as libraries, and then we specify in the `.cabal` file
how to build the executables/test suites using them. I settled on this
approach as:

- it works well with IDE integration (in particular, I have found that
  [HLS][hls] doesn't like being used in executable or test code, so we dodge
  that problem by exposing everything as library code).
- it has the added benefit that you can load up every bit of code (including
  test suites) in a REPL to play around with, which can be nice during
  development.
- using `mixins` in the cabal file we can reduce the boilerplate that might
  otherwise be needed to structure things this way.

## Usage

### Stack

```shell
> stack build
> stack run day-01
> stack test :test-01
```

```shell
> stack repl adventofcode2021:lib
...
>>> :load Day01
```

### Cabal

```shell
> cabal build
> cabal run day-01
> cabal run test-01
```

```shell
> cabal repl adventofcode2021
...
>>> :load Day01
```

[hls]: https://github.com/haskell/haskell-language-server
