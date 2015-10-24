{-
Parser of Context-Free Grammar

Language: GHC 7.6, Haskell 2010
-}

{-# LANGUAGE Rank2Types #-}

module Text.Parser
(
	Item (Result, Scan), Pattern (Pattern), Parser,
	prepare, scan, cases,
	Grammar, forms,
	build, results, parse,
	anything, satisfy, match,
	string, optional, oneOf, noneOf)
where

import Prelude ()
import Data.Bool (Bool)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Function (($), (.))
import Data.List (concat, elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap))
import Control.Applicative
	(
		Applicative (pure, (<*>)), liftA2,
		Alternative (empty, (<|>), some, many))
import Data.Traversable (traverse)
import Control.Monad (Monad (return), (=<<), (>>), mapM, foldM)
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

newtype Pattern s c r a = Pattern ((a -> Set s c r) -> Set s c r)

bind_Pattern :: (a -> Set s c r) -> Pattern s c r a -> Set s c r
bind_Pattern k (Pattern p) = p k

instance Functor (Pattern s c r) where
	fmap f (Pattern x) = Pattern (x . (. f))

instance Applicative (Pattern s c r) where
	pure x = Pattern ($ x)
	Pattern f <*> Pattern x = Pattern (f . (x .) . (.))

data Parser s c r = Parser [Item s c r] (STRef s (ST s ()))

mapSTlist :: (a -> ST s [b]) -> [a] -> ST s [b]
mapSTlist = (fmap concat .) . mapM

prepare :: Pattern s c r r -> STRef s (ST s ()) -> ST s (Parser s c r)
prepare (Pattern p) cleanupR
	= do
		items <- p (\ r -> return [Result r])
		return (Parser items cleanupR)

scan :: Parser s c r -> c -> ST s (Parser s c r)
scan (Parser items_0 cleanupR) c
	= do
		cleanup <- readSTRef cleanupR
		writeSTRef cleanupR (return ())
		cleanup
		items_1 <- mapSTlist (\ i -> scan_with_Item i c) items_0
		return (Parser items_1 cleanupR)

cases :: [Pattern s c r a] -> Pattern s c r a
cases ps = Pattern (\ k -> mapSTlist (bind_Pattern k) ps)

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

type Grammar s c r a = ReaderT (STRef s (ST s ())) (ST s) a

forms :: [Pattern s c r a] -> Grammar s c r (Pattern s c r a)
forms patterns
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
								in mapSTlist (bind_Pattern add_result) patterns
					_ -> mapSTlist continuation =<< get_results memo
		return (Pattern pattern_cps)

build :: Grammar s c r (Pattern s c r r) -> ST s (Parser s c r)
build syntax
	= do
		cleanup <- newSTRef (return ())
		pattern <- runReaderT syntax cleanup
		prepare pattern cleanup

results :: Parser s c r -> [r]
results (Parser items _) = result_of_Item =<< items

parse :: (forall s. Grammar s c r (Pattern s c r r)) -> [c] -> [r]
parse syntax input
	= runST
		(do
			parser <- build syntax
			parsed <- foldM scan parser input
			return (results parsed))

anything :: Pattern s c r c
anything = Pattern (\ k -> return [Scan k])

satisfy :: (c -> Bool) -> Pattern s c r c
satisfy f
	= Pattern (\ k -> return [Scan (\ c -> if f c then k c else return [])])

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

instance Alternative (Pattern s c r) where
	empty = Pattern (\ _ -> return [])
	p1 <|> p2 = cases [p1, p2]
	many pattern = let p = cases [pure [], fmap (:) pattern <*> p] in p
	some pattern = fmap (:) pattern <*> many pattern
