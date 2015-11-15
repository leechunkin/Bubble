{-
Basic Regular Expression
-}

{-# LANGUAGE RecursiveDo #-}

module Text.Parser.Bubble.RegEx
	(
		Config, nomagic, basic, extended,
		RegExRT, RegExRP, RegExRG, RegExT,
		regex)
where

import Text.Parser.Bubble

import Prelude (undefined, (/=), succ, pred, (+), (*))
import Data.Bool (Bool (True, False), not, (&&))
import Data.Char (Char)
import Data.Word (Word)
import Data.String (String)
import Data.Tuple (fst, snd)
import Data.Function (($), (.))
import Data.List ((++), concat, foldl')
import Data.Functor ((<$>), (<$))
import Control.Applicative
	(
		Applicative (pure, (<*>), (<*), (*>)), liftA, liftA2, liftA3,
		(<|>), many, some)
import Control.Monad (Monad (return), (=<<), liftM, liftM2, liftM3, sequence)
import Control.Monad.State
	(State, state, runState, evalState, get, put, modify)
import Data.Ord (max)

data Config
	= Config {
		escape    :: Char,
		circumfex :: (Bool, Char),
		dollar    :: (Bool, Char),
		star      :: (Bool, Char),
		dot       :: (Bool, Char),
		bracket0  :: (Bool, Char),
		bracket1  :: Char,
		inverse   :: Char,
		paren0    :: (Bool, Char),
		paren1    :: (Bool, Char),
		bar       :: (Bool, Char),
		plus      :: (Bool, Char),
		question  :: (Bool, Char)
	}

nomagic :: Config
nomagic
	= Config {
		escape    = '\\',
		circumfex = (True,  '^'),
		dollar    = (True,  '$'),
		star      = (False, '*'),
		dot       = (False, '.'),
		bracket0  = (False, '['),
		bracket1  = ']',
		inverse   = '^',
		paren0    = (False, '('),
		paren1    = (False, ')'),
		bar       = (False, '|'),
		plus      = (False, '+'),
		question  = (False, '?')
	}

basic :: Config
basic
	= Config {
		escape    = '\\',
		circumfex = (True,  '^'),
		dollar    = (True,  '$'),
		star      = (True,  '*'),
		dot       = (True,  '.'),
		bracket0  = (True,  '['),
		bracket1  = ']',
		inverse   = '^',
		paren0    = (False, '('),
		paren1    = (False, ')'),
		bar       = (False, '|'),
		plus      = (False, '+'),
		question  = (False, '?')
	}

extended :: Config
extended
	= Config {
		escape    = '\\',
		circumfex = (True, '^'),
		dollar    = (True, '$'),
		star      = (True, '*'),
		dot       = (True, '.'),
		bracket0  = (True, '['),
		bracket1  = ']',
		inverse   = '^',
		paren0    = (True, '('),
		paren1    = (True, ')'),
		bar       = (True, '|'),
		plus      = (True, '+'),
		question  = (True, '?')
	}

specialCharacters :: [Config -> (Bool, Char)]
specialCharacters = [star, dot, bracket0, paren0, paren1, bar, plus, question]

metaCharacters :: Config -> Bool -> Bool -> [Char]
metaCharacters config startP endP
	= (\ r
		-> if startP && fst (circumfex config)
			then snd (circumfex config) : r
			else r)
	$ (\ r
		-> if endP && fst (dollar config)
			then snd (dollar config) : r
			else r)
	$ foldl'
		(\ r f
			-> case f config of
				(True,  c) -> c : r
				(False, _) -> r)
		[escape config]
		specialCharacters

quotedCharacters :: Config -> Bool -> Bool -> [Char]
quotedCharacters config startP endP
	= (\ r
		-> if startP && not (fst (circumfex config))
			then snd (circumfex config) : r
			else r)
	$ (\ r
		-> if endP && not (fst (dollar config))
			then snd (dollar config) : r
			else r)
	$ foldl'
		(\ r f
			-> case f config of
				(True,  _) -> r
				(False, c) -> c : r)
		[escape config]
		specialCharacters

{----------------------------------------------------------------------------}

setElt :: a -> a -> Word -> [a] -> [a]
setElt _ x 0 []      = [x]
setElt _ x 0 (_ : r) = x : r
setElt z x i []      = z : setElt z x (pred i) []
setElt z x i (f : r) = f : setElt z x (pred i) r

{----------------------------------------------------------------------------}

type RegExRT = State [String] String

returnRT :: String -> RegExRT
returnRT = return

emptyRT :: RegExRT
emptyRT = returnRT ""

type RegExRP s r = Pattern s Char r RegExRT

pureRP :: RegExRT -> RegExRP s r
pureRP = return

emptyRP :: RegExRP s r
emptyRP = pureRP emptyRT

type RegExRG s r = Form s (RegExRP s r)

returnRG :: RegExRP s r -> RegExRG s r
returnRG = return

emptyRG :: RegExRG s r
emptyRG = returnRG emptyRP

type RegExT s r = State Word (RegExRG s r)

returnT :: RegExRG s r -> RegExT s r
returnT = return

emptyT :: RegExT s r
emptyT = return emptyRG

regex :: Config -> Grammar s Char r (RegExRG ss rr)
regex config
	= mdo
		let
			meta field
				= case field config of
					(True,  c) -> () <$ match c
					(False, c) -> () <$ match (escape config) <* match c
			mapseq f x = f <$> sequence x
			append2' = liftM2 (liftM2 (liftA2 (liftM2 (++))))
			append3' = liftM3 (liftM3 (liftA3 (liftM3 (\ a b c -> a ++ b ++ c))))
			pad' = returnT (liftM (emptyRT <$) (many_' anything))
		let ordinary startP endP
			= let char' c = returnT (returnRG (returnRT . (: []) <$> match c))
				in cases
					[ char'
						<$> noneOf (metaCharacters config startP endP)
					, char'
						<$ match (escape config)
						<*> noneOf (quotedCharacters config startP endP)
					]
		bracket <- forms
			[ (\ c s -> returnT (returnRG (returnRT . (: "") <$> oneOf (c : s))))
				<$ meta bracket0
				<*> noneOf [bracket1 config, inverse config]
				<*> many (noneOf [inverse config])
				<* match (bracket1 config)
			, (\ c s -> returnT (returnRG (returnRT . (: "") <$> oneOf (c : s))))
				<$ meta bracket0
				<*> match (bracket1 config)
				<*> many (noneOf [bracket1 config])
				<* match (bracket1 config)
			, (\ s -> returnT (returnRG (returnRT . (: "") <$> noneOf s)))
				<$ meta bracket0
				<* match (inverse config)
				<*> some (noneOf [bracket1 config])
				<* match (bracket1 config)
			, (\ c s -> returnT (returnRG (returnRT . (: "") <$> noneOf (c : s))))
				<$ meta bracket0
				<* match (inverse config)
				<*> match (bracket1 config)
				<*> many (noneOf [bracket1 config])
				<* match (bracket1 config)
			]
		let atom startP endP
			= let
				padRP pattern
					= case startP of
						True
							-> case endP of
								True  -> many anything *> pattern <* many anything
								False -> many anything *> pattern
						False
							-> case endP of
								True  ->                  pattern <* many anything
								False ->                  pattern
				anyRP = returnT (returnRG (returnRT . (: []) <$> anything))
				in liftM (liftM padRP) <$> cases
					[ ordinary startP endP
					, anyRP <$ meta dot
					, bracket
					]
		let
			branches' reT brT
				= state
					(\ s
						-> let
							(reRG, s2re) = runState reT s
							(brRG, s2br) = runState brT s
							in (liftM2 (<|>) reRG brRG, max s2re s2br))
			group' pT
				= do
					n <- get
					put (succ n)
					(liftM . liftM . liftA)
						(\ pRT
							-> do
								pR <- pRT
								modify (setElt "" pR n)
								returnRT pR)
						pT
			repeat0' = liftM ((liftM (liftA (mapseq concat))) . (many' =<<))
			repeat1' = liftM ((liftM (liftA (mapseq concat))) . (some' =<<))
			optional' = liftM (liftM (emptyRP <|>))
		nonnullpiece <- forms
			[ atom False False
			, group' <$ meta paren0 <*> nonnullexpr <* meta paren1
			, append2'
				<$> nonnullpiece
				<*> (repeat0' <$> nonnullpiece <* meta star)
			, repeat1' <$> nonnullpiece <* meta plus
			, append2'
				<$> nonnullpiece
				<*> (optional' <$> nonnullpiece <* meta question)
			]
		{-
		nonnullbranch <- forms
			[ append2' <$> nonnullbranch <*> nonnullpiece
			, pure emptyT
			]
		-}
		nonnullbranch
			<- liftM (liftA (mapseq (mapseq (mapseq (mapseq concat)))))
				(some' nonnullpiece)
		nonnullexpr <- forms
			[ nonnullbranch
			, branches' <$> nonnullexpr <* meta bar <*> nonnullbranch
			]
		repeating <- forms
			[ repeat0' <$> nonnullpiece <* meta star
			, repeat1' <$> nonnullpiece <* meta plus
			, optional' <$> nonnullpiece <* meta question
			]
		piece00 <- forms
			[ atom False False
			, group' <$ meta paren0 <*> expr00 <* meta paren1
			, repeating
			]
		piece10 <- forms
			[ atom True False
			, group' <$ meta paren0 <*> expr10 <* meta paren1
			, repeating
			]
		piece01 <- forms
			[ atom False True
			, group' <$ meta paren0 <*> expr01 <* meta paren1
			, repeating
			]
		piece11 <- forms
			[ atom True True
			, group' <$ meta paren0 <*> expr11 <* meta paren1
			, repeating
			]
		{-
		branch00 <- forms
			[ append2' <$> branch00 <*> piece00
			, pure emptyT
			]
		-}
		branch00
			<- liftM (liftA (mapseq (mapseq (mapseq (mapseq concat)))))
				(many' piece00)
		branch10 <- forms
			[ meta circumfex *> branch00
			, append2' <$> piece10 <*> branch00
			, pure emptyT
			]
		branch01 <- forms
			[ branch00 <* meta dollar
			, append2' <$> branch00 <*> piece01
			, pure emptyT
			]
		branch11 <- forms
			[ meta circumfex *> branch01
			, branch10 <* meta dollar
			, append3' <$> piece10 <*> branch00 <*> piece01
			, piece11
			, pad' <$ meta dollar
			, pure pad'
			]
		expr00 <- forms
			[ branch00
			, branches' <$> expr00 <* meta bar <*> branch00
			]
		expr10 <- forms
			[ branch10
			, branches' <$> expr10 <* meta bar <*> branch10
			]
		expr01 <- forms
			[ branch01
			, branches' <$> expr01 <* meta bar <*> branch01
			]
		expr11 <- forms
			[ branch11
			, branches' <$> expr11 <* meta bar <*> branch11
			]
		return ((\ s -> evalState s 0) <$> expr11)
