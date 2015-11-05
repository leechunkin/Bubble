{-
Parser of Context-Free Grammar

Language: GHC 7.6, Haskell 2010
-}

{-# LANGUAGE Rank2Types #-}

module Text.Parser.Bubble
(
	Item (Result, Scan),
	Pattern (Pattern), cases, satisfy,
	Parser (Parser), prepare, scan, failed, results, feed,
	Memo (Memo), Forms (Forms), askForms, liftForms,
	Grammar, form, forms, form_, forms_, build, parse,
	anything, match, string, oneOf, noneOf,
	many_, some_, many', some', many_', some_')
where

import Prelude ()
import Data.Bool (Bool (True, False))
import Data.Either (Either (Left, Right))
import Data.Function (($), (.), const, flip)
import Data.List ((++), concat, reverse, null, elem, notElem)
import Data.Eq (Eq ((==)))
import Data.Functor (Functor (fmap), (<$>), (<$))
import Control.Applicative
	(
		Applicative (pure, (<*>)), liftA2,
		Alternative (empty, (<|>)), (<*))
import Data.Traversable (traverse)
import Control.Monad (Monad (return, (>>=)), (=<<), (>>), liftM, liftM2)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.ST (ST, runST, fixST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

data Item s c r = Result r | Scan (c -> ST s [Item s c r])

result_of_Item :: Item s c r -> [r]
result_of_Item (Result r) = [r]
result_of_Item _ = []

scan_with_Item :: Item s c r -> c -> ST s [Item s c r]
scan_with_Item (Scan f) c = f c
scan_with_Item _ _ = return []

newtype Pattern s c r a
	= Pattern ((a -> ST s [Item s c r]) -> ST s [Item s c r])

instance Functor (Pattern s c r) where
	fmap f (Pattern x) = Pattern (x . (. f))

instance Applicative (Pattern s c r) where
	pure x = Pattern ($ x)
	Pattern f <*> Pattern x = Pattern (f . (x .) . (.))

instance Monad (Pattern s c r) where
	return = pure
	Pattern x >>= f = Pattern (\ k -> x ((\ (Pattern p) -> p k) . f))

mapSTlist :: (a -> ST s [b]) -> [a] -> ST s [b]
mapSTlist = (liftM concat .) . traverse

cases :: [Pattern s c r a] -> Pattern s c r a
cases ps = Pattern (\ k -> mapSTlist (\ (Pattern p) -> p k) ps)

instance Alternative (Pattern s c r) where
	empty = Pattern (\ _ -> return [])
	Pattern p1 <|> Pattern p2 = Pattern (\ k -> liftM2 (++) (p1 k) (p2 k))

satisfy :: (c -> Bool) -> Pattern s c r c
satisfy f
	= Pattern
		(\ k -> return [Scan (\ c -> if f c then k c else return [])])

data Parser s c r = Parser [Item s c r] (STRef s (ST s ()))

prepare :: Pattern s c r r -> STRef s (ST s ()) -> ST s (Parser s c r)
prepare (Pattern pattern) cleanupR
	= do
		items <- pattern (\ r -> return [Result r])
		return (Parser items cleanupR)

scan :: Parser s c r -> c -> ST s (Parser s c r)
scan (Parser items_0 cleanupR) c
	= do
		cleanup <- readSTRef cleanupR
		writeSTRef cleanupR (return ())
		cleanup
		items_1 <- mapSTlist (\ i -> scan_with_Item i c) items_0
		return (Parser items_1 cleanupR)

failed :: Parser s c r -> Bool
failed (Parser p _) = null p

results :: Parser s c r -> [r]
results (Parser items _) = result_of_Item =<< items

feed :: Parser s c r -> [c] -> ST s (Either [c] (Parser s c r))
feed parser input =
	if failed parser
		then return (Left input)
		else case input of
			[]    -> return (Right parser)
			c : s -> (\ p -> feed p s) =<< scan parser c

newtype Forms s a = Forms (STRef s (ST s()) -> ST s a)

instance Functor (Forms s) where
	fmap f (Forms x) = Forms (fmap f . x)

instance Applicative (Forms s) where
	pure = Forms . const . return
	Forms f <*> Forms x = Forms (\ e -> f e <*> x e)

instance Monad (Forms s) where
	return = pure
	Forms x >>= f = Forms (\ e -> x e >>= ((\ (Forms y) -> y e) . f))

instance MonadFix (Forms s) where
	mfix f = Forms (\ e -> fixST ((\ (Forms r') -> r' e) . f))

askForms :: Forms s (STRef s (ST s ()))
askForms = Forms return

liftForms :: ST s a -> Forms s a
liftForms = Forms . const

type Grammar s c r a = Forms s (Pattern s c r a)

data Memo s c r a = Memo (STRef s [a -> ST s [Item s c r]]) (STRef s [a])

make_Memo :: ST s (Memo s c r a)
make_Memo = liftA2 Memo (newSTRef []) (newSTRef [])

get_continuations_Memo :: Memo s c r a -> ST s [a -> ST s [Item s c r]]
get_continuations_Memo (Memo csr _) = readSTRef csr

get_results_Memo :: Memo s c r a -> ST s [a]
get_results_Memo (Memo _ rsr) = readSTRef rsr

push_continuation_Memo :: Memo s c r a -> (a -> ST s [Item s c r]) -> ST s ()
push_continuation_Memo (Memo csr _) c = modifySTRef' csr (c :)

push_result_Memo :: Memo s c r a -> a -> ST s ()
push_result_Memo (Memo _ rsr) c = modifySTRef' rsr (c :)

form :: Pattern s c r a -> Grammar s c r a
form (Pattern pattern)
	= do
		cleanupR <- askForms
		memoR <- liftForms (newSTRef =<< make_Memo)
		let cps continuation
			= do
				memo <- readSTRef memoR
				memo_continuations <- get_continuations_Memo memo
				push_continuation_Memo memo continuation
				case memo_continuations of
					[]
						-> do
							let clear_MemoR = writeSTRef memoR =<< make_Memo
								in modifySTRef' cleanupR (clear_MemoR >>)
							let add_result new_result
								= do
									push_result_Memo memo new_result
									continuations <- get_continuations_Memo memo
									mapSTlist ($ new_result) continuations
								in pattern add_result
					_ : _ -> mapSTlist continuation =<< get_results_Memo memo
		return (Pattern cps)

forms :: [Pattern s c r a] -> Grammar s c r a
forms = form . cases

data Memo0 s c r = Memo0 (STRef s [() -> ST s [Item s c r]]) (STRef s Bool)

make_Memo0 :: ST s (Memo0 s c r)
make_Memo0 = liftA2 Memo0 (newSTRef []) (newSTRef False)

get_continuations_Memo0 :: Memo0 s c r -> ST s [() -> ST s [Item s c r]]
get_continuations_Memo0 (Memo0 csr _) = readSTRef csr

get_resulted_ :: Memo0 s c r -> ST s Bool
get_resulted_ (Memo0 _ rr) = readSTRef rr

push_continuation_Memo0 :: Memo0 s c r -> (() -> ST s [Item s c r]) -> ST s ()
push_continuation_Memo0 (Memo0 csr _) c = modifySTRef' csr (c :)

set_resulted_Memo0 :: Memo0 s c r -> ST s ()
set_resulted_Memo0 (Memo0 _ rr) = writeSTRef rr True

form_ :: Pattern s c r () -> Grammar s c r ()
form_ (Pattern pattern)
	= do
		cleanupR <- askForms
		memoR <- liftForms (newSTRef =<< make_Memo0)
		let cps continuation
			= do
				memo <- readSTRef memoR
				memo_continuation <- get_continuations_Memo0 memo
				push_continuation_Memo0 memo continuation
				case memo_continuation of
					[]
						-> do
							let clear_memoR = writeSTRef memoR =<< make_Memo0
								in modifySTRef' cleanupR (clear_memoR >>)
							let some_result _
								= do
									set_resulted_Memo0 memo
									mapSTlist ($ ()) =<< get_continuations_Memo0 memo
								in pattern some_result
					_ : _
						-> do
							resulted <- get_resulted_ memo
							case resulted of
								True  -> continuation ()
								False -> return []
		return (Pattern cps)

forms_ :: [Pattern s c r ()] -> Grammar s c r ()
forms_ = form_ . cases

build :: Grammar s c r r -> ST s (Parser s c r)
build (Forms grammar)
	= do
		cleanup <- newSTRef (return ())
		pattern <- grammar cleanup
		prepare pattern cleanup

parse :: (forall s. Grammar s c r r) -> [c] -> Either [c] [r]
parse grammar input
	= runST
		(do
			parser <- build grammar
			parsed <- feed parser input
			case parsed of
				Left  s -> return (Left s)
				Right p -> return (Right (results p)))

anything :: Pattern s c r c
anything = Pattern (\ k -> return [Scan k])

match :: Eq c => c -> Pattern s c r c
match = satisfy . (==)

string :: Eq c => [c] -> Pattern s c r [c]
string = traverse match

oneOf :: Eq c => [c] -> Pattern s c r c
oneOf s = satisfy (\ c -> elem c s)

noneOf :: Eq c => [c] -> Pattern s c r c
noneOf s = satisfy (\ c -> notElem c s)

many_ :: Pattern s c r a -> Pattern s c r ()
many_ pattern = let p = cases [pure (), () <$ pattern <* p] in p

some_ :: Pattern s c r a -> Pattern s c r ()
some_ pattern = () <$ pattern <* many_ pattern

many' :: Pattern s c r a -> Grammar s c r [a]
many' pattern
	= fmap
		(fmap reverse)
		(mfix
			(\ grammar
				-> form
					$   pure []
					<|> flip (:) <$> grammar <*> pattern))

some' :: Pattern s c r a -> Grammar s c r [a]
some' pattern
	= fmap
		(fmap reverse)
		(mfix
			(\ grammar
				-> form
					$   (: []) <$> pattern
					<|> flip (:) <$> grammar <*> pattern))

many_' :: Pattern s c r a -> Grammar s c r ()
many_' pattern
	= mfix
		(\ grammar
			-> form_
				$   pure ()
				<|> grammar <* pattern)

some_' :: Pattern s c r a -> Grammar s c r ()
some_' pattern
	= mfix
		(\ grammar
			-> form_
				$   () <$ pattern
				<|> grammar <* pattern)
