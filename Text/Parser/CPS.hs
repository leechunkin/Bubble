{-
Parser of Context-Free Grammar
Using recursive descent method in continuation-passing style

Language: GHC 7.6, Haskell 2010
-}

module Text.Parser.CPS
(
	Item (Result, Scan), Pattern (Pattern), Parser,
	prepare, scan, cases,
	Syntax, forms,
	build, through, results, parse,
	anything, satisfy, match,
	string, optional, oneOf, noneOf)
where

import Prelude ()
import Data.Bool (Bool)
import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe (Nothing, Just))
import Data.Function (($), (.), const)
import Data.List (elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap))
import Control.Applicative
	(
		Applicative (pure, (<*>)),
		Alternative (empty, (<|>)))
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

instance Monad (Pattern c r) where
	return = pure
	Pattern cps >>= f =
		Pattern (\ k -> cps (\ x -> let (Pattern y) = f x in y k))

type Parser c r = [Item c r]

prepare :: Pattern c r r -> Parser c r
prepare (Pattern p) = p (\ r -> [Result r])

scan :: Parser c r -> c -> Parser c r
scan items c = items >>= \ i -> scan_with_Item i c

cases :: [Pattern c r a] -> Pattern c r a
cases ps = Pattern (\ k -> ps >>= \ (Pattern p) -> p k)

instance Alternative (Pattern c r) where
	empty = Pattern (\ _ -> [])
	p1 <|> p2 = cases [p1, p2]

satisfy :: (c -> Bool) -> Pattern c r c
satisfy f = Pattern (\ k -> [Scan (\ c -> if f c then k c else [])])

type Syntax c r a = Identity (Pattern c r a)

forms :: [Pattern c r a] -> Syntax c r a
forms = return . cases

build :: Syntax c r r -> Parser c r
build = prepare . runIdentity

through :: Pattern c r r -> [c] -> Either [c] (Parser c r)
through pattern input
	= let
		feed s        [] = Left s
		feed []       p  = Right p
		feed (c : s)  p  = feed s (scan p c)
		in feed input (prepare pattern)

results :: Parser c r -> [r]
results = (>>= result_of_Item)

parse :: Syntax c r r -> [c] -> Either [c] [r]
parse syntax input
	= case through (runIdentity syntax) input of
		Left  remainer -> Left remainer
		Right parser   -> Right (results parser)

anything :: Pattern c r c
anything = Pattern (\ k -> [Scan k])

match :: Eq c => c -> Pattern c r c
match = satisfy . (==)

string :: Eq c => [c] -> Pattern c r [c]
string = traverse match

optional :: Pattern c r a -> Pattern c r (Maybe a)
optional pattern = cases [pure Nothing, fmap Just pattern]

oneOf :: Eq c => [c] -> Pattern c r c
oneOf s = satisfy (\ c -> elem c s)

noneOf :: Eq c => [c] -> Pattern c r c
noneOf s = satisfy (\ c -> notElem c s)
