{-
Simple calculator with ambiguous grammar
-}

{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Text.Parser
import Text.Parser.Common

import Data.List
import Control.Applicative

syntax :: Syntax s Char r Double
{-
syntax ::= ws expr ws
ws     ::= space*
atom   ::= float
         | "(" ws expr ws ")"
expr   ::= term
         | expr ws "*" ws expr
         | expr ws "/" ws expr
         | expr ws "+" ws expr
         | expr ws "-" ws expr
-}
syntax
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
					case parse syntax input of
						Left _ -> putStrLn "Syntax error"
						Right answers -> print (sort (nub answers))
					main
