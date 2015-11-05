# Bubble

## Introduction

Bubble is context-free grammar parser library for Haskell.
It uses a modified Earley parser algorithm.

* All context-free grammars can be parsed, including ambiguous and
unambigous grammars.
* No backtracking is used.
Linear time for almost all LR(k) grammars, and cubic time for worst
case ambigouous grammars.
* The grammar is built on-the-fly.
It provides good flexiblity to use.
* No lexer is needed (lexerless parsing) although you can use one.
* There is no constraint or restriction on input and output types.
Any type can be used for input or output and not necessary to be an
instance of any typeclass.
* The internal structure structure is quite simple, which makes it easy
to use, understand and extend.
* Some context-sensitive grammars can be parsed by using the flexibility
of the parser and the ST monad underlayer.

## Compatibility and dependencies

The library is written in Haskell 2010 with Rank2Types extension.
Rank2Types is used for ST monad.
RecursiveDo is suggested for convenience in using.

GHC 7.6 base library is used in development.
