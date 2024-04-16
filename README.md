# tagless-interpreter

## Description

This project implements different interpreters for an embedded programming language.

The language has the following features:

- booleans, if-then-else
- pairs and projections
- lambda, application
- integers, addition, multiplication, (unary) minus
- ordering (i.e. less than) on integers
- equality on integers
- a combinator for computing fixed points 'fix'

The interpreters are the following:

- `PLInterpreter`: as a usual programming language (i.e. that 'runs')
- `LengthInterpreter`: that computes the length of the program
- `HaskellRepInterpreter`: that computes (using `Data.Text`) a valid Haskell representation of the program
- `PrettyPrintInterpreter`: that computes (using `Data.Text`) a "pretty-printed" version of the program

### Version 2

In version 2 I added the following:

- `Compiler`: a compiler (using Template Haskell's typed quotes; see below for details)
- `TracingInterpreter`: a tracing interpreter (i.e. like an interpreter, but it prints out all the steps)
- Improvements to `PrettyPrintInterpreter` so that it prints on multiple lines. 
- `PartialEval`: a partial evaluator

I also tried to implement an abstract interpretor but I didn't manage to make it work in time.
It is fully written, but untested and commented out.
The starter code didn't compile for me and I could not figure it out.

## Setup

The only setup needed is the installation of Stack, instructions for which can be found at this url: https://docs.haskellstack.org/en/stable/install_and_upgrade/

## Running the project

Running the project will execute an example that uses all 4 interpreters.

You can run it with the following command:
```
stack run
```

## Running the tests

You can run the tests with the following command:
```
stack test
```

## Project structure

### Main 

The main (`app/Main.hs`) only contains an example program that is interpreted with the 4 interpreters.

### Interpreters

The Symantics of the language are defined in `src/Symantics.hs`.

The differents interpreters that implement Symantics are in the `src/interpreters/` directory.

### Tests

The tests are implemented using `HUnit`.
The differents interpreters each have a test file in the `test/` directory.
The `Spec.hs` file is used to run all of the tests.
