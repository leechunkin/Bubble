{-
Strict parser of JavaScript Object Notation (RFC 7158)
-}

module Text.Parser.Bubble.Json
(
	JsonValue
		(
			JsonFalse, JsonNull, JsonTrue,
			JsonNumber, JsonString,
			JsonObject, JsonArray),
	false, null, true, number, string, object, array,
	value, text, showJsonString, showJson)
where

import Text.Parser.Bubble hiding (string)
import qualified Text.Parser.Bubble as Parser (string)

import Prelude (
	Num (fromInteger), Double,
	toInteger, negate, (+), (-), (*), (^^))
import Data.Bool (Bool (True, False), (&&))
import Data.Bits (shift)
import Data.Char (Char, chr, digitToInt, isDigit, isHexDigit)
import Data.List (foldr, foldl', (++), length)
import Data.String (String)
import Data.Functor ((<$>), (<$))
import Control.Applicative (pure, (<*>), (<*), (*>), many, some)
import Data.Function ((.))
import Data.Ord ((<=), (>=))
import Text.Show (Show (show))

data JsonValue
	= JsonFalse
	| JsonNull
	| JsonTrue
	| JsonNumber Double
	| JsonString String
	| JsonObject [(String, JsonValue)]
	| JsonArray [JsonValue]

false :: Pattern s Char r JsonValue
false = JsonFalse <$ Parser.string "false"

null :: Pattern s Char r JsonValue
null = JsonNull <$ Parser.string "null"

true :: Pattern s Char r JsonValue
true = JsonTrue <$ Parser.string "true"

number :: Pattern s Char r JsonValue
number
	= let
		digit = satisfy isDigit
		digit9 = satisfy (\ c -> c >= '1' && c <= '9')
		minus = cases
			[ pure False
			, True <$ match '-'
			]
		int = cases
			[ (: []) <$> match '0'
			, (:) <$> digit9 <*> many digit
			]
		frac = cases
			[ pure ""
			, "" <$ match '.'
			, match '.' *> some digit
			]
		exp = cases
			[ pure ('+', "")
			, (,) <$ oneOf "Ee" <*> cases [pure '+', oneOf "+-"] <*> some digit
			]
		num minus' int' frac' (es', exp')
			= let
				strToInteger = foldl' (\ n d -> n*10 + toInteger (digitToInt d)) 0
				strToNum = fromInteger . strToInteger
				sign x = if minus' then negate x else x
				i = strToNum int'
				f = strToNum frac' * (10 ^^ negate (length frac'))
				esign x = case es' of {'-' -> negate x; _ -> x}
				e = 10 ^^ esign (strToInteger exp')
			in JsonNumber (sign ((i + f) * e))
		in num <$> minus <*> int <*> frac <*> exp

str :: Pattern s Char r String
str
	= let
		unescaped = noneOf "\"\\"
		hex4ToInt a b c d
			= shift (digitToInt a) 12
				+ shift (digitToInt b) 8
				+ shift (digitToInt c) 4
				+ digitToInt d
		hex4ToChar a b c d = chr (hex4ToInt a b c d)
		hex = satisfy isHexDigit
		escaped = match '\\' *> cases
			[ '"'  <$ match '"'
			, '\\' <$ match '\\'
			, '/'  <$ match '/'
			, '\b' <$ match 'b'
			, '\f' <$ match 'f'
			, '\n' <$ match 'n'
			, '\r' <$ match 'r'
			, '\t' <$ match 't'
			]
		basic = Parser.string "\\u" *> cases
			[ hex4ToChar <$> oneOf "0123456789AaBbCcEeFf" <*> hex <*> hex <*> hex
			, hex4ToChar <$> oneOf "Dd" <*> oneOf "01234567" <*> hex <*> hex
			]
		hex8ToChar a b c d e f g h
			= let
				lead = hex4ToInt a b c d
				trail = hex4ToInt e f g h
				in chr (shift (lead - 0xD800) 10 + (trail - 0xDC00) + 0x10000)
		supplementary
			= hex8ToChar
				<$ Parser.string "\\u"
				<*> oneOf "Dd" <*> oneOf "89AaBb" <*> hex <*> hex
				<* Parser.string "\\u"
				<*> oneOf "Dd" <*> oneOf "CcDdEeFf" <*> hex <*> hex
		char = cases [unescaped, escaped, basic, supplementary]
		body = cases ["" <$ match '"', (:) <$> char <*> body]
	in match '"' *> body

string :: Pattern s Char r JsonValue
string = JsonString <$> str

ws :: Pattern s Char r ()
ws = () <$ many (oneOf " \t\n\r")

array :: Pattern s Char r JsonValue
array
	= let
		values = cases
			[ (: []) <$ ws <*> value <* ws <* match ']'
			, (:) <$ ws <*> value <* ws <* match ',' <*> values
			]
		in JsonArray <$ match '[' <*> cases
			[ [] <$ ws <* match ']'
			, values
			]

object :: Pattern s Char r JsonValue
object
	= let
		member = (,) <$> str <* ws <* match ':' <* ws <*> value
		members = cases
			[ (: []) <$ ws <*> member <* ws <* match '}'
			, (:) <$ ws <*> member <* ws <* match ',' <*> members
			]
		in JsonObject <$ match '{' <*> cases
			[ [] <$ ws <* match '}'
			, members
			]

value :: Pattern s Char r JsonValue
value = cases
	[ false
	, null
	, true
	, object
	, array
	, number
	, string
	]

text :: Pattern s Char r JsonValue
text = ws *> value <* ws

showJsonString :: String -> String
showJsonString s
	= let
		escape '"'  r = '\\' : '"'  : r
		escape '\\' r = '\\' : '\\' : r
		escape '\b' r = '\\' : 'b'  : r
		escape '\f' r = '\\' : 'f'  : r
		escape '\t' r = '\\' : 't'  : r
		escape '\n' r = '\\' : 'n'  : r
		escape '\r' r = '\\' : 'r'  : r
		escape c r = c : r
		in '"' : foldr escape "\"" s

showJson :: JsonValue -> String
showJson JsonFalse = "false"
showJson JsonNull = "null"
showJson JsonTrue = "true"
showJson (JsonObject []) = "{}"
showJson (JsonObject (m1 : ms))
	= let
		show_member (n, v) = showJsonString n ++ ':' : showJson v
		more_member m s = ',' : show_member m ++ s
		in '{' : show_member m1 ++ foldr more_member "}" ms
showJson (JsonArray []) = "[]"
showJson (JsonArray (v1 : vs))
	= '[' : showJson v1 ++ foldr (\ v s -> ',' : showJson v ++ s) "]" vs
showJson (JsonNumber f) = show f
showJson (JsonString s) = showJsonString s

instance Show JsonValue where
	show = showJson
