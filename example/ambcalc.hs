{-
Simple calculator with ambiguous grammar
-}

{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Text.Parser.Bubble
import Text.Parser.Bubble.Common

import Data.List
import Control.Applicative

grammar :: Grammar s Char r Double
{-
grammar ::= ws expr ws
ws      ::= space*
atom    ::= float
          | "(" ws expr ws ")"
expr    ::= term
          | expr ws "*" ws expr
          | expr ws "/" ws expr
          | expr ws "+" ws expr
          | expr ws "-" ws expr
-}
grammar
	= mdo
		let ws = many space
		atom <- forms
			[ float
			, match '(' *> ws *> expr <* ws <* match ')'
			]
		expr <- forms
			[ atom
			, (*) <$> expr <* ws <* match '*' <* ws <*> expr
			, (/) <$> expr <* ws <* match '/' <* ws <*> expr
			, (+) <$> expr <* ws <* match '+' <* ws <*> expr
			, (-) <$> expr <* ws <* match '-' <* ws <*> expr
			]
		return (ws *> expr <* ws)

main :: IO ()
main
	= do
		putStr "? "
		input <- getLine
		case input of
			[] -> putStrLn "Done"
			_
				-> do
					case parse grammar input of
						Left _ -> putStrLn "Syntax error"
						Right answers -> print (sort (nub answers))
					main
