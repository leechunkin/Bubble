{-
Parser of Context-Free Grammar
Using recursive descent method in continuation-passing style

Language: GHC 7.6, Haskell 2010

* Wed, 14 Oct 2015 17:27:14 +0800
  Start
* Fri, 16 Oct 2015 00:29:29 +0800
  Version 1 and rename to ParserCPS.hs
* Thu, 22 Oct 2015 14:23:00 +0800
  Organized as a package
-}

module Text.Parser.CPS
(
	Item (Result, Scan), Pattern (Pattern), Parser,
	prepare, scan, cases,
	Grammar, forms,
	build, through, results, parse,
	anything, satisfy, match,
	string, oneOf, noneOf)
where

import Prelude ()
import Data.Function (($), (.), const)
import Data.Bool (Bool)
import Data.Maybe (Maybe (Nothing, Just))
import Data.List (foldl', elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap))
import Control.Applicative
	(
		Applicative (pure, (<*>)),
		Alternative (empty, (<|>), some, many))
import Data.Traversable (traverse)
import Control.Monad (Monad (return, (>>=)))
import Data.Functor.Identity (Identity (runIdentity))

data Item c r = Result r | Scan (c -> [Item c r])

result_of_Item :: Item c r -> [r]
result_of_Item (Result r) = [r]
result_of_Item _ = []

scan_with_Item :: Item c r -> c -> [Item c r]
scan_with_Item (Scan f) = f
scan_with_Item _ = const []

newtype Pattern c r a = Pattern ((a -> [Item c r]) -> [Item c r])

instance Functor (Pattern c r) where
	fmap f (Pattern x) = Pattern (x . (. f))

instance Applicative (Pattern c r) where
	pure x = Pattern ($ x)
	Pattern f <*> Pattern x = Pattern (f . (x .) . (.))

type Parser c r = [Item c r]

prepare :: Pattern c r r -> Parser c r
prepare (Pattern p) = p (\ r -> [Result r])

scan :: Parser c r -> c -> Parser c r
scan items c = items >>= \ i -> scan_with_Item i c

cases :: [Pattern c r a] -> Pattern c r a
cases ps = Pattern (\ k -> ps >>= \ (Pattern p) -> p k)

type Grammar c r a = Identity a

forms :: [Pattern c r a] -> Grammar c r (Pattern c r a)
forms = return . cases

build :: Grammar c r (Pattern c r r) -> Parser c r
build = prepare . runIdentity

through :: Pattern c r r -> [c] -> Parser c r
through pattern input = foldl' scan (prepare pattern) input

results :: Parser c r -> [r]
results = (>>= result_of_Item)

parse :: Grammar c r (Pattern c r r) -> [c] -> [r]
parse = (results .) . through . runIdentity

anything :: Pattern c r c
anything = Pattern (\ k -> [Scan k])

satisfy :: (c -> Bool) -> Pattern c r c
satisfy f = Pattern (\ k -> [Scan (\ c -> if f c then k c else [])])

match :: Eq c => c -> Pattern c r c
match = satisfy . (==)

string :: Eq c => [c] -> Pattern c r [c]
string = traverse match

optional :: Pattern s c r a -> Pattern s c r (Maybe a)
optional pattern = cases [pure Nothing, fmap Just pattern]

oneOf :: Eq c => [c] -> Pattern c r c
oneOf s = satisfy (\ c -> elem c s)

noneOf :: Eq c => [c] -> Pattern c r c
noneOf s = satisfy (\ c -> notElem c s)

instance Alternative (Pattern c r) where
	empty = Pattern (\ _ -> [])
	p1 <|> p2 = cases [p1, p2]
	many pattern = let p = cases [pure [], fmap (:) pattern <*> p] in p
	some pattern = fmap (:) pattern <*> many pattern
