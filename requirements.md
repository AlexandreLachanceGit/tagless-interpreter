# Requirements

Build interpreters for a language with the following features:

    - booleans, if-then-else
    - pairs and projections
    - lambda, application
    - integers, addition, multiplication, (unary) minus
    - ordering (i.e. less than) on integers
    - equality on integers
    - a combinator for computing fixed points 'fix'

The interpreters to build (in finally tagless style) using Haskell:

    - as a usual programming language (i.e. that 'runs')
    - that computes the length of the program
    - that computes (using `Data.Text`) a valid Haskell representation of the program
    - that computes (using `Data.Text`) a "pretty-printed" version of the program

Your code should come with a (commented!) test suite of programs that test all the features and all the interpreters. Use a standard unit test framework (such as HUnit or HTF) for that purpose.

Your code should be organized using either Stack or Cabal for doing builds. You should provide a README.md file documenting how to build your project and where to find the right pieces.

Your code should be your own. You may collaborate to figure out the 'technology' aspects (test unit frameworks, stack, cabal, Haskell itself). Any kind of AI assistant use is strictly forbidden.

Bonus: 

    - factor out common code, for each instance, as much as possible.
    - add additional non-trivial instances (types, abstract interpreters, compiler to a lower-level embedded DSL, etc)

