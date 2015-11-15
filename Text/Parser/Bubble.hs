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
	Memo (Memo), Form (Form), runForm, askForm, liftForm,
	Grammar, form, forms, build, parse,
	anything, match, string, oneOf, noneOf,
	many_, some_, many', some', many_', some_')
where

import Prelude ()
import Data.Bool (Bool)
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
import Control.Monad
	(
		Monad (return, (>>=)), (=<<), (>>), liftM, liftM2,
		MonadPlus (mzero, mplus))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.ST (ST, runST, fixST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

data Item s c r = Result r | Scan (c -> ST s [Item s c r])

result_Item :: Item s c r -> [r]
result_Item (Result r) = [r]
result_Item _ = []

scan_Item :: Item s c r -> c -> ST s [Item s c r]
scan_Item (Scan f) c = f c
scan_Item _ _ = return []

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

instance MonadPlus (Pattern s c r) where
	mzero = empty
	mplus = (<|>)

satisfy :: (c -> Bool) -> Pattern s c r c
satisfy f
	= Pattern
		(\ k -> return [Scan (\ c -> if f c then k c else return [])])

data Parser s c r = Parser [Item s c r] (STRef s (ST s ()))

prepare :: Pattern s c r r -> STRef s (ST s ()) -> ST s (Parser s c r)
prepare (Pattern pattern) cleanupV
	= do
		items <- pattern (\ r -> return [Result r])
		return (Parser items cleanupV)

scan :: Parser s c r -> c -> ST s (Parser s c r)
scan (Parser items_0 cleanupV) c
	= do
		cleanup <- readSTRef cleanupV
		writeSTRef cleanupV (return ())
		cleanup
		items_1 <- mapSTlist (\ i -> scan_Item i c) items_0
		return (Parser items_1 cleanupV)

failed :: Parser s c r -> Bool
failed (Parser p _) = null p

results :: Parser s c r -> [r]
results (Parser items _) = result_Item =<< items

feed :: Parser s c r -> [c] -> ST s (Either [c] (Parser s c r))
feed parser input =
	if failed parser
		then return (Left input)
		else case input of
			[]    -> return (Right parser)
			c : s -> (\ p -> feed p s) =<< scan parser c

newtype Form s a = Form (STRef s (ST s()) -> ST s a)

runForm :: Form s a -> STRef s (ST s()) -> ST s a
runForm (Form f) = f

instance Functor (Form s) where
	fmap f (Form x) = Form (fmap f . x)

instance Applicative (Form s) where
	pure = Form . const . return
	Form f <*> Form x = Form (\ e -> f e <*> x e)

instance Monad (Form s) where
	return = pure
	Form x >>= f = Form (\ e -> x e >>= (flip runForm e . f))

instance MonadFix (Form s) where
	mfix f = Form (\ e -> fixST (flip runForm e . f))

askForm :: Form s (STRef s (ST s ()))
askForm = Form return

liftForm :: ST s a -> Form s a
liftForm = Form . const

type Grammar s c r a = Form s (Pattern s c r a)

data Memo s c r a = Memo (STRef s [a -> ST s [Item s c r]]) (STRef s [a])
	deriving Eq

make_Memo :: ST s (Memo s c r a)
make_Memo = liftA2 Memo (newSTRef []) (newSTRef [])

get_continuations :: Memo s c r a -> ST s [a -> ST s [Item s c r]]
get_continuations (Memo csr _) = readSTRef csr

get_results :: Memo s c r a -> ST s [a]
get_results (Memo _ rsr) = readSTRef rsr

push_continuation :: Memo s c r a -> (a -> ST s [Item s c r]) -> ST s ()
push_continuation (Memo csr _) c = modifySTRef' csr (c :)

clear_results :: Memo s c r a -> ST s ()
clear_results (Memo _ rsr) = writeSTRef rsr []

push_result :: Memo s c r a -> a -> ST s ()
push_result (Memo _ rsr) c = modifySTRef' rsr (c :)

form :: Pattern s c r a -> Grammar s c r a
form (Pattern pattern)
	= do
		cleanupV <- askForm
		memoV <- liftForm (newSTRef =<< make_Memo)
		let cps continuation
			= do
				memo <- readSTRef memoV
				memo_continuations <- get_continuations memo
				push_continuation memo continuation
				case memo_continuations of
					[]
						-> do
							let clear_MemoV
								= do
									writeSTRef memoV =<< make_Memo
									clear_results memo
								in modifySTRef' cleanupV (clear_MemoV >>)
							let add_result new_result
								= do
									memo_latest <- readSTRef memoV
									if memo == memo_latest
										then push_result memo new_result
										else return ()
									continuations <- get_continuations memo
									mapSTlist ($ new_result) continuations
								in pattern add_result
					_ : _ -> mapSTlist continuation =<< get_results memo
		return (Pattern cps)

forms :: [Pattern s c r a] -> Grammar s c r a
forms = form . cases

build :: Grammar s c r r -> ST s (Parser s c r)
build (Form grammar)
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
				Right p
					-> case results p of
						[] -> return (Left [])
						rs -> return (Right rs))

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
many_ pattern = let p = pure () <|> () <$ pattern <* p in p

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
			-> form
				$   pure ()
				<|> grammar <* pattern)

some_' :: Pattern s c r a -> Grammar s c r ()
some_' pattern
	= mfix
		(\ grammar
			-> form
				$   () <$ pattern
				<|> grammar <* pattern)
