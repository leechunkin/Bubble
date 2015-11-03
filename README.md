# Bubble

## Introduction

Bubble is (mostly) a combinator parser library for Haskell.
It uses modified Earley parser algorithm, and is designed to parse
context-free grammars.

* All context-free grammars can be parsed, including ambiguous and
unambigous grammars.
* No backtracking is used. Linear time for almost all LL(k) and LR(k)
grammars, and O(n³) for worst case ambigouous grammars.
* Combinator parser builds the grammar "on-the-fly".
It gives a lot of flexibility to use.
* The internal structure structure is quite simple.
It makes the library easy to use and extend.
* Some context-sensitive grammars can be parsed due to the flexibility
and ST monad it runs on.

## Compatibility and dependencies

The library is written in Haskell 2010 and GHC 7.6 library,
with Rank2Types extension for ST monad. MTL package is required to
provide ReaderT, ContT and ST monads.
