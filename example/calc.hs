{-
Simple calculator
-}

{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Text.Parser
import Text.Parser.Common

import Control.Applicative

grammar :: Grammar s Char r Double
{-
grammar ::= ws expr ws
ws      ::= space*
atom    ::= float
          | "(" ws expr ws ")"
term    ::= atom
          | term ws "*" ws atom
          | term ws "/" ws atom
expr    ::= term
          | expr ws "+" ws term
          | expr ws "-" ws term
-}
grammar
	= mdo
		let ws = many space
		atom <- forms
			[ float
			, match '(' *> ws *> expr <* ws <* match ')'
			]
		term <- forms
			[ atom
			, (*) <$> term <* ws <* match '*' <* ws <*> atom
			, (/) <$> term <* ws <* match '/' <* ws <*> atom
			]
		expr <- forms
			[ term
			, (+) <$> expr <* ws <* match '+' <* ws <*> term
			, (-) <$> expr <* ws <* match '-' <* ws <*> term
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
				-> case parse grammar input of
					Left _ -> putStrLn "Syntax error" >> main
					Right [answer] -> print answer >> main
					Right _ -> putStrLn "Parser error"
