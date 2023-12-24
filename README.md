# haculator

simple arithmetic expression repl

## usage

clone this repo and run `cabal run haculator`.

currently, there are two major features in the repl:

1. evaluation of arithmetic expressions consisting of only addition,
   subtraction, multiplication, division, negation, and parentheses;
2. solving of linear equations involving one variable.

powered by [haskeline](https://hackage.haskell.org/package/haskeline), the repl
allows for most features you would expect in any rich terminal interface, like
scrolling through history.

## roadmap
i have a few plans for the project moving forward:
- [ ] add exponentiation and logarithms to the expression grammar
- [ ] make a toy programming language with the calculator as a foundation

## credit
this project started as an exercise to see how far i could get parsing
arithmetic expressions knowing next to nothing about programming language
theory. i wrote a 
[small amount](https://joshcbrown.github.io/posts/expression-parser.html)
after finishing an MVP and then read the excellent
[Design patterns for parser combinators (functional pearl)](https://dl.acm.org/doi/10.1145/3471874.3472984)
on recommendation.
the paper largely inspired the direction of the project from that point forward, and want to thank the authors for exposing the elegance of parser combinators so thoroughly.
