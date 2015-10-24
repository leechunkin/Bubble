{-
Common Patterns
-}

module Text.Parser.Common
(
	repeating, atmost,
	newline, tab, space, printable, unprintable,
	digit, octDigit, hexDigit,
	letter, upper, lower, alphaNum,
	decimal, integer, octal, hexadecimal,
	unsigned, quotedString)
where

import Text.Parser

import Prelude
	(
		Integral(toInteger),
		Num (fromInteger, (+), (*), negate),
		Integer, ($!), fromIntegral, pred)
import Data.Functor ((<$>), (<$))
import Control.Applicative (pure, (<*>), (<*), (*>), some)
import Data.Eq ((/=))
import Data.Bool (Bool (False), not, (&&))
import Data.Char
	(
		Char, chr,
		isSpace, isPrint,
		isDigit, isOctDigit, isHexDigit,
		isLetter, isUpper, isLower, isAlphaNum,
		digitToInt)
import Data.Word (Word)
import Data.List (foldl', (++))
import Data.String (String)

repeating :: Integral i => i -> Pattern s c r a -> Pattern s c r [a]
repeating i p
	= let
		r 0 = pure []
		r j = (:) <$> p <*> (r $! pred j)
	in r (fromIntegral i :: Word)

atmost :: Integral i => i -> Pattern s c r a -> Pattern s c r [a]
atmost i p
	= let
		r 0 = pure []
		r j = cases [pure [], (:) <$> p <*> (r $! pred j)]
	in r (fromIntegral i :: Word)

newline :: Pattern s Char r Char
newline = match '\n'

tab :: Pattern s Char r Char
tab = match '\t'

space :: Pattern s Char r Char
space = satisfy isSpace

printable :: Pattern s Char r Char
printable = satisfy isPrint

unprintable :: Pattern s Char r Char
unprintable = satisfy (\ c -> not (isPrint c))

digit :: Pattern s Char r Char
digit = satisfy isDigit

octDigit :: Pattern s Char r Char
octDigit = satisfy isOctDigit

hexDigit :: Pattern s Char r Char
hexDigit = satisfy isHexDigit

letter :: Pattern s Char r Char
letter = satisfy isLetter

upper :: Pattern s Char r Char
upper = satisfy isUpper

lower :: Pattern s Char r Char
lower = satisfy isLower

alphaNum :: Pattern s Char r Char
alphaNum = satisfy isAlphaNum

decimal :: Pattern s Char r Integer
decimal = foldl' (\ n d -> n*10 + toInteger (digitToInt d)) 0 <$> some digit

integer :: Pattern s Char r Integer
integer
	= let
		sign = cases
			[ pure False
			, match '+' *> sign
			, not <$ match '-' <*> sign
			]
		in (\ s n -> if s then negate n else n) <$> sign <*> decimal

octal :: Pattern s Char r Integer
octal =
	foldl' (\ n d -> n*8 + toInteger (digitToInt d)) 0
		<$> some octDigit

hexadecimal :: Pattern s Char r Integer
hexadecimal =
	foldl' (\ n d -> n*16 + toInteger (digitToInt d)) 0
		<$> some hexDigit

unsigned :: Pattern s Char r Integer
unsigned = cases
	[ decimal
	, string "0O" *> octal
	, string "0o" *> octal
	, string "0X" *> hexadecimal
	, string "0x" *> hexadecimal
	]

quotedString :: Pattern s Char r String
quotedString
	= let
		charesc = cases
			[ "\a" <$ match 'a'
			, "\b" <$ match 'b'
			, "\f" <$ match 'f'
			, "\n" <$ match 'n'
			, "\r" <$ match 'r'
			, "\t" <$ match 't'
			, "\v" <$ match 'v'
			, "\\" <$ match '\\'
			, "\'" <$ match '\''
			, "\"" <$ match '\"'
			, ""   <$ match '&'
			]
		cntrl = match '^' *> cases
			[ '\^@' <$ match '@'
			, '\^A' <$ match 'A'
			, '\^B' <$ match 'B'
			, '\^C' <$ match 'C'
			, '\^D' <$ match 'D'
			, '\^E' <$ match 'E'
			, '\^F' <$ match 'F'
			, '\^G' <$ match 'G'
			, '\^H' <$ match 'H'
			, '\^I' <$ match 'I'
			, '\^J' <$ match 'J'
			, '\^K' <$ match 'K'
			, '\^L' <$ match 'L'
			, '\^M' <$ match 'M'
			, '\^N' <$ match 'N'
			, '\^O' <$ match 'O'
			, '\^P' <$ match 'P'
			, '\^Q' <$ match 'Q'
			, '\^R' <$ match 'R'
			, '\^S' <$ match 'S'
			, '\^T' <$ match 'T'
			, '\^U' <$ match 'U'
			, '\^V' <$ match 'V'
			, '\^W' <$ match 'W'
			, '\^X' <$ match 'X'
			, '\^Y' <$ match 'Y'
			, '\^Z' <$ match 'Z'
			, '\^[' <$ match '['
			, '\^\' <$ match '\\'
			, '\^]' <$ match ']'
			, '\^^' <$ match '^'
			, '\^_' <$ match '_'
			]
		ascii = cases
			[ cntrl
			, '\NUL' <$ string "NUL"
			, '\SOH' <$ string "SOH"
			, '\STX' <$ string "STX"
			, '\ETX' <$ string "ETX"
			, '\EOT' <$ string "EOT"
			, '\ENQ' <$ string "ENQ"
			, '\ACK' <$ string "ACK"
			, '\BEL' <$ string "BEL"
			, '\BS'  <$ string "BS"
			, '\HT'  <$ string "HT"
			, '\LF'  <$ string "LF"
			, '\VT'  <$ string "VT"
			, '\FF'  <$ string "FF"
			, '\CR'  <$ string "CR"
			, '\SO'  <$ string "SO"
			, '\SI'  <$ string "SI"
			, '\DLE' <$ string "DLE"
			, '\DC1' <$ string "DC1"
			, '\DC2' <$ string "DC2"
			, '\DC3' <$ string "DC3"
			, '\DC4' <$ string "DC4"
			, '\NAK' <$ string "NAK"
			, '\SYN' <$ string "SYN"
			, '\ETB' <$ string "ETB"
			, '\CAN' <$ string "CAN"
			, '\EM'  <$ string "EM"
			, '\SUB' <$ string "SUB"
			, '\ESC' <$ string "ESC"
			, '\FS'  <$ string "FS"
			, '\GS'  <$ string "GS"
			, '\RS'  <$ string "RS"
			, '\US'  <$ string "US"
			, '\SP'  <$ string "SP"
			, '\DEL' <$ string "DEL"
			]
		charFromInteger n = chr (fromInteger n)
		normal c = c /= '"' && c /= '\\'
		dec = charFromInteger <$> decimal
		oct = charFromInteger <$ match 'o' <*> octal
		hex = charFromInteger <$ match 'x' <*> hexadecimal
		undec = satisfy (\ c -> normal c && not (isDigit c))
		unoct = satisfy (\ c -> normal c && not (isOctDigit c))
		unhex = satisfy (\ c -> normal c && not (isHexDigit c))
		afterdec = cases ["" <$ match '"', escaped, (:) <$> undec <*> body]
		afteroct = cases ["" <$ match '"', escaped, (:) <$> unoct <*> body]
		afterhex = cases ["" <$ match '"', escaped, (:) <$> unhex <*> body]
		gap = () <$ some space <* match '\\'
		escaped = match '\\' *> cases
			[ (++) <$> charesc <*> body
			, (:) <$> ascii <*> body
			, (:) <$> dec <*> afterdec
			, (:) <$> oct <*> afteroct
			, (:) <$> hex <*> afterhex
			, gap *> body
			]
		unescaped = satisfy normal
		body = cases ["" <$ match '"', escaped, (:) <$> unescaped <*> body]
		in match '"' *> body
