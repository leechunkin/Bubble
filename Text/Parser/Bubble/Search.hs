{-
Searcher and Partial Parsing
-}

{-# LANGUAGE Rank2Types #-}

module Text.Parser.Bubble.Search
(
	collect, prefixes, search,
	feed_minimal, parse_shortest, feed_maximal, parse_longest)
where

import Text.Parser.Bubble

import Prelude ()
import Data.List (map, (++))
import Data.Either (Either (Left, Right))
import Data.Functor (fmap)
import Control.Monad (return)
import Control.Monad.ST (ST, runST)

collect :: Parser s c r -> [c] -> ST s [(r, [c])]
collect p  []
	= return (map (\ r -> (r, [])) (results p))
collect p0 (c : cs)
	= do
		p1 <- scan p0 c
		let rs = map (\ r -> (r, cs)) (results p1)
		fmap (rs ++) (collect p1 cs)

prefixes :: (forall s. Grammar s c r r) -> [c] -> [(r, [c])]
prefixes grammar input
	= runST
		(do
			parser <- build grammar
			collect parser input)

search :: (forall s. Grammar s c r r) -> [c] -> [(r, [c], [c])]
search parser input
	= (map (\ (r, s) -> (r, input, s)) (prefixes parser input))
		++ case input of
			[] -> []
			(_: s) -> search parser s

partial_parse
	:: (forall s. Parser s c r -> [c] -> ST s (Either [c] (Parser s c r, [c])))
		-> (forall s. Grammar s c r r) -> [c] -> Either [c] ([r], [c])
partial_parse feeder grammar input
	= runST
		(do
			parser_0 <- build grammar
			fed <- feeder parser_0 input
			case fed of
				Left  remaining
					-> return (Left remaining)
				Right (parser_1, remaining)
					-> return (Right (results parser_1, remaining)))

feed_minimal :: Parser s c r -> [c] -> ST s (Either [c] (Parser s c r, [c]))
feed_minimal parser input
	= case results parser of
		[] -> case input of
			[] -> return (Left [])
			c : cs
				-> do
					scanned <- scan parser c
					feed_minimal scanned cs
		_  -> return (Right (parser, input))

parse_shortest :: (forall s. Grammar s c r r) -> [c] -> Either [c] ([r], [c])
parse_shortest = partial_parse feed_minimal

feed_maximal :: Parser s c r -> [c] -> ST s (Either [c] (Parser s c r, [c]))
feed_maximal parser input
	= case input of
		[] -> return (Left [])
		c : cs
			-> do
				scanned <- scan parser c
				case results parser of
					[] -> feed_maximal scanned cs
					_
						-> do
							fed <- feed_maximal scanned cs
							case fed of
								Left  _ -> return (Right (parser, input))
								Right _ -> return fed

parse_longest :: (forall s. Grammar s c r r) -> [c] -> Either [c] ([r], [c])
parse_longest = partial_parse feed_maximal
