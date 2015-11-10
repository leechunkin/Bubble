{-
Example of parsing context-sensitive grammar
-}

module Main (main) where

import Text.Parser.Bubble
import Text.Parser.Bubble.Common

grammar :: Grammar s Char r ()
grammar
	= let pattern
		= do
			n <- decimal
			_ <- match '*'
			c <- anything
			_ <- match '='
			_ <- times n (match c)
			return ()
		in return pattern

main :: IO ()
main
	= do
		putStr "? "
		input <- getLine
		case input of
			[] -> putStrLn "Done"
			_
				-> case parse grammar input of
					Left _ -> putStrLn "Mismatch" >> main
					Right [_] -> putStrLn "Match" >> main
					Right _ -> putStrLn "Parser error"
