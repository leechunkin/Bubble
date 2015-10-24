{-
Common Patterns
-}

module Text.Parser.Common
(
	repeating, atmost,
	newline, tab, space, printable, unprintable,
	digit, octDigit, hexDigit,
	letter, upper, lower, alphaNum,
	decimal, integer, octal, hexadecimal, unsignedInteger, signedInteger,
	float, unsignedFloat, signedFloat, quotedString)
where

import Text.Parser

import Prelude
	(
		Integral(toInteger),
		Num (fromInteger, (+), (*), negate),
		Integer, Double,
		($!), fromIntegral, pred, (^^))
import Data.Bool (Bool (True, False), not, (&&))
import Data.Char
	(
		Char, chr,
		isSpace, isPrint,
		isDigit, isOctDigit, isHexDigit,
		isLetter, isUpper, isLower, isAlphaNum,
		digitToInt)
import Data.Word (Word)
import Data.List (foldl', (++), length)
import Data.String (String)
import Data.Eq ((/=))
import Data.Functor ((<$>), (<$))
import Control.Applicative (pure, (<*>), (<*), (*>), some, many)

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

convert_decimal :: String -> Integer
convert_decimal = foldl' (\ n d -> n*10 + toInteger (digitToInt d)) 0

decimal :: Pattern s Char r Integer
decimal = convert_decimal <$> some digit

sign :: Pattern s Char r a -> Pattern s Char r (Bool, a)
sign following
	= let loop = cases
		[ match '+' *> loop
		, (\ (s, x) -> (not s, x)) <$ match '-' <*> loop
		, (\ x -> (False, x)) <$> following
		]
		in loop

signing :: Num n => (Bool, n) -> n
signing (False, n) = n
signing (True,  n) = negate n

integer :: Pattern s Char r Integer
integer = signing <$> sign decimal

octal :: Pattern s Char r Integer
octal =
	foldl' (\ n d -> n*8 + toInteger (digitToInt d)) 0
		<$> some octDigit

hexadecimal :: Pattern s Char r Integer
hexadecimal =
	foldl' (\ n d -> n*16 + toInteger (digitToInt d)) 0
		<$> some hexDigit

unsignedInteger :: Pattern s Char r Integer
unsignedInteger = cases
-- Integer number literal in Haskell 2010
	[ decimal
	, string "0O" *> octal
	, string "0o" *> octal
	, string "0X" *> hexadecimal
	, string "0x" *> hexadecimal
	]

signedInteger :: Pattern s Char r Integer
signedInteger = signing <$> sign unsignedInteger

convert_float :: Bool -> String -> Bool -> String -> String -> Double
convert_float neg int inv exp frac
	= let
		i = fromInteger (convert_decimal int)
		e = convert_decimal exp
		f = fromInteger (convert_decimal frac)
		p = length frac
		in signing (neg, ((i + (f * 10 ^^ negate p)) * 10 ^^ signing (inv, e)))

float :: Pattern s Char r Double
float
	= cases
		[ (\ (n, i) f (v, e) -> convert_float n i v e f)
			<$> sign (some digit)
				<*> cases [pure "", match '.' *> many digit]
				<*> cases [pure (False, ""), oneOf "Ee" *> sign (many digit)]
		, (\ (n, f) (v, e) -> convert_float n "" v e f)
			<$> sign (match '.' *> many digit)
				<*> cases [pure (False, ""), oneOf "Ee" *> sign (many digit)]
		, (\ (n, i) (v, e) f -> convert_float n i v e f)
			<$> sign (many digit)
				<* oneOf "Ee" <*> sign (many digit)
				<* match '.' <*> many digit
		]

unsignedFloat :: Pattern s Char r Double
unsignedFloat
-- Floating point number literal in Haskell 2010
	= (\ i f (v, e) -> convert_float False i v e f)
		<$> some digit
			<* match '.' <*> some digit
			<*> cases
				[ pure (False, "")
				, (\ s e -> ('-' /= s, e)) <$
					oneOf "Ee" <*> cases [pure '+', oneOf "+-"] <*> many digit
				]

signedFloat :: Pattern s Char r Double
signedFloat = signing <$> sign unsignedFloat

quotedString :: Pattern s Char r String
quotedString
-- String literal in Haskell 2010
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
		nonescape c = c /= '"' && c /= '\\'
		dec = charFromInteger <$> decimal
		oct = charFromInteger <$ match 'o' <*> octal
		hex = charFromInteger <$ match 'x' <*> hexadecimal
		undec = satisfy (\ c -> nonescape c && not (isDigit c))
		unoct = satisfy (\ c -> nonescape c && not (isOctDigit c))
		unhex = satisfy (\ c -> nonescape c && not (isHexDigit c))
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
		unescaped = satisfy nonescape
		body = cases ["" <$ match '"', escaped, (:) <$> unescaped <*> body]
		in match '"' *> body
