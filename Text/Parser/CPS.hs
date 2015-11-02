{-
Parser of Context-Free Grammar
Using recursive descent method in continuation-passing style

Language: GHC 7.6, Haskell 2010
-}

module Text.Parser.CPS
(
	Item (Result, Scan),
	Pattern (Pattern), cases, satisfy,
	Parser, prepare, scan, failed, results, feed,
	Grammar, forms, build, parse,
	anything, match, string, oneOf, noneOf,
	many_, some_, many', some', many_', some_')
where

import Prelude ()
import Data.Bool (Bool)
import Data.Either (Either (Left, Right))
import Data.Function (($), (.), const)
import Data.List (null, elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap), (<$))
import Control.Applicative
	(
		Applicative (pure, (<*>)),
		Alternative (empty, (<|>)), (<*), many, some)
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

cases :: [Pattern c r a] -> Pattern c r a
cases ps = Pattern (\ k -> ps >>= \ (Pattern p) -> p k)

instance Alternative (Pattern c r) where
	empty = Pattern (\ _ -> [])
	p1 <|> p2 = cases [p1, p2]

satisfy :: (c -> Bool) -> Pattern c r c
satisfy f = Pattern (\ k -> [Scan (\ c -> if f c then k c else [])])

type Parser c r = [Item c r]

prepare :: Pattern c r r -> Parser c r
prepare (Pattern p) = p (\ r -> [Result r])

scan :: Parser c r -> c -> Parser c r
scan items c = items >>= \ i -> scan_with_Item i c

failed :: Parser c r -> Bool
failed = null

results :: Parser c r -> [r]
results = (>>= result_of_Item)

feed :: Parser c r -> [c] -> Either [c] (Parser c r)
feed parser input =
	if failed parser
		then Left input
		else case input of
			[]    -> Right parser
			c : s -> feed (scan parser c) s

type Grammar s c r a = Identity (Pattern c r a)

forms :: [Pattern c r a] -> Grammar s c r a
forms = return . cases

build :: Grammar s c r r -> Parser c r
build = prepare . runIdentity

parse :: Grammar s c r r -> [c] -> Either [c] [r]
parse grammar input
	= case feed (build grammar) input of
		Left  remainer -> Left remainer
		Right parser   -> Right (results parser)

anything :: Pattern c r c
anything = Pattern (\ k -> [Scan k])

match :: Eq c => c -> Pattern c r c
match = satisfy . (==)

string :: Eq c => [c] -> Pattern c r [c]
string = traverse match

oneOf :: Eq c => [c] -> Pattern c r c
oneOf s = satisfy (\ c -> elem c s)

noneOf :: Eq c => [c] -> Pattern c r c
noneOf s = satisfy (\ c -> notElem c s)

many_ :: Pattern c r a -> Pattern c r ()
many_ pattern = let p = cases [pure (), () <$ pattern <* p] in p

some_ :: Pattern c r a -> Pattern c r ()
some_ pattern = () <$ pattern <* many_ pattern

many' :: Pattern c r a -> Grammar s c r [a]
many' = return . many

some' :: Pattern c r a -> Grammar s c r [a]
some' = return . some

many_' :: Pattern c r a -> Grammar s c r ()
many_' = return . many_

some_' :: Pattern c r a -> Grammar s c r ()
some_' = return . some_
