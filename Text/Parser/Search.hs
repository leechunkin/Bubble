{-# LANGUAGE Rank2Types #-}

module Text.Parser.Search
(
	collect, prefixes, search)
where

import Text.Parser

import Prelude ()
import Data.List (map, (++))
import Data.Functor (fmap)
import Control.Monad (return)
import Control.Monad.ST (ST, runST)

collect :: Parser s c r -> [c] -> ST s [(r, [c])]
collect p []
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
