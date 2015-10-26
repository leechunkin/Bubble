{-
Parser of Context-Free Grammar

Language: GHC 7.6, Haskell 2010
-}

{-# LANGUAGE Rank2Types #-}

module Text.Parser
(
	Item (Result, Scan), Pattern (Pattern), Parser,
	prepare, scan, cases, results,
	Memo (Memo), Syntax, memoize, forms, build, parse,
	anything, satisfy, match,
	string, optional, oneOf, noneOf)
where

import Prelude ()
import Data.Bool (Bool)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Either (Either (Left, Right))
import Data.Function (($), (.))
import Data.List (concat, elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap))
import Control.Applicative
	(
		Applicative (pure, (<*>)), liftA2,
		Alternative (empty, (<|>)))
import Data.Traversable (traverse)
import Control.Monad (Monad (return, (>>=)), (=<<), (>>), mapM)
import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

data Item s c r = Result r | Scan (c -> ST s [Item s c r])

result_of_Item :: Item s c r -> [r]
result_of_Item (Result r) = [r]
result_of_Item _ = []

scan_with_Item :: Item s c r -> c -> ST s [Item s c r]
scan_with_Item (Scan f) c = f c
scan_with_Item _ _ = return []

type Set s c r = ST s [Item s c r]

newtype Pattern s c r a = Pattern (Cont (Set s c r) a)

cont_of_Pattern :: Pattern s c r a -> Cont (Set s c r) a
cont_of_Pattern (Pattern p) = p

bind_Pattern :: (a -> Set s c r) -> Pattern s c r a -> Set s c r
bind_Pattern k p = runCont (cont_of_Pattern p) k

instance Functor (Pattern s c r) where
	fmap f x = Pattern (fmap f (cont_of_Pattern x))

instance Applicative (Pattern s c r) where
	pure x = Pattern (pure x)
	f <*> x = Pattern (cont_of_Pattern f <*> cont_of_Pattern x)

instance Monad (Pattern s c r) where
	return = pure
	x >>= f = Pattern (cont_of_Pattern . f =<< cont_of_Pattern x)

data Parser s c r = Parser [Item s c r] (STRef s (ST s ()))

mapSTlist :: (a -> ST s [b]) -> [a] -> ST s [b]
mapSTlist = (fmap concat .) . mapM

prepare :: Pattern s c r r -> STRef s (ST s ()) -> ST s (Parser s c r)
prepare p cleanupR
	= do
		items <- runCont (cont_of_Pattern p) (\ r -> return [Result r])
		return (Parser items cleanupR)

scan :: Parser s c r -> c -> ST s (Parser s c r)
scan (Parser items_0 cleanupR) c
	= do
		cleanup <- readSTRef cleanupR
		writeSTRef cleanupR (return ())
		cleanup
		items_1 <- mapSTlist (\ i -> scan_with_Item i c) items_0
		return (Parser items_1 cleanupR)

results :: Parser s c r -> [r]
results (Parser items _) = result_of_Item =<< items

cases :: [Pattern s c r a] -> Pattern s c r a
cases ps = Pattern (cont (\ k -> mapSTlist (bind_Pattern k) ps))

instance Alternative (Pattern s c r) where
	empty = Pattern (cont (\ _ -> return []))
	p1 <|> p2 = cases [p1, p2]

satisfy :: (c -> Bool) -> Pattern s c r c
satisfy f
	= Pattern
		(cont (\ k -> return [Scan (\ c -> if f c then k c else return [])]))

data Memo s c r a = Memo (STRef s [a -> Set s c r]) (STRef s [a])

make_Memo :: ST s (Memo s c r a)
make_Memo = liftA2 Memo (newSTRef []) (newSTRef [])

get_continuations :: Memo s c r a -> ST s [a -> Set s c r]
get_continuations (Memo csr _) = readSTRef csr

get_results :: Memo s c r a -> ST s [a]
get_results (Memo _ rsr) = readSTRef rsr

push_continuation :: Memo s c r a -> (a -> Set s c r) -> ST s ()
push_continuation (Memo csr _) c = modifySTRef' csr (c :)

push_result :: Memo s c r a -> a -> ST s ()
push_result (Memo _ rsr) c = modifySTRef' rsr (c :)

type Syntax s c r a = ReaderT (STRef s (ST s ())) (ST s) (Pattern s c r a)

memoize :: Pattern s c r a -> Syntax s c r a
memoize pattern
	= do
		memoR <- lift (newSTRef =<< make_Memo)
		cleanupR <- ask
		let pattern_cps continuation
			= do
				memo <- readSTRef memoR
				memo_continuations <- get_continuations memo
				push_continuation memo continuation
				case memo_continuations of
					[]
						-> do
							let clean_MemoR = writeSTRef memoR =<< make_Memo
								in modifySTRef' cleanupR (clean_MemoR >>)
							let add_result new_result
								= do
									push_result memo new_result
									continuations <- get_continuations memo
									mapSTlist ($ new_result) continuations
								in runCont (cont_of_Pattern pattern) add_result
					_ -> mapSTlist continuation =<< get_results memo
		return (Pattern (cont pattern_cps))

forms :: [Pattern s c r a] -> Syntax s c r a
forms = memoize . cases

build :: Syntax s c r r -> ST s (Parser s c r)
build syntax
	= do
		cleanup <- newSTRef (return ())
		pattern <- runReaderT syntax cleanup
		prepare pattern cleanup

parse :: (forall s. Syntax s c r r) -> [c] -> Either [c] [r]
parse syntax input
	= runST
		(do
			parser <- build syntax
			let
				feed s         (Parser [] _) = return (Left s)
				feed []      p               = return (Right (results p))
				feed (c : s) p@(Parser _  _) = feed s =<< scan p c
			feed input parser)

anything :: Pattern s c r c
anything = Pattern (cont (\ k -> return [Scan k]))

match :: Eq c => c -> Pattern s c r c
match = satisfy . (==)

string :: Eq c => [c] -> Pattern s c r [c]
string = traverse match

optional :: Pattern s c r a -> Pattern s c r (Maybe a)
optional pattern = cases [pure Nothing, fmap Just pattern]

oneOf :: Eq c => [c] -> Pattern s c r c
oneOf s = satisfy (\ c -> elem c s)

noneOf :: Eq c => [c] -> Pattern s c r c
noneOf s = satisfy (\ c -> notElem c s)
