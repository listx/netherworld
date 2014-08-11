{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Util where

import "monads-tf" Control.Monad.Identity
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy ()
import Text.Parsec.Token

enclose :: (String, String) -> String -> String
enclose (a, b) str = a ++ str ++ b

paren :: String -> String
paren = enclose ("(", ")")

squote :: String -> String
squote = enclose ("`", "'")

squote' :: String -> String
squote' = enclose (" ", " ") . squote

pshow :: Show a => a -> IO ()
pshow = putStrLn . show

downcase :: String -> String
downcase = map toLower

enumsHash :: Show a => [a] -> [(String, a)]
enumsHash enums = zip (map (downcase . show) enums) enums

gameDataFormatDef :: GenLanguageDef T.Text u Identity
gameDataFormatDef = LanguageDef
	{ commentStart   = "/*"
	, commentEnd     = "*/"
	, commentLine    = "#"
	, nestedComments = False
	, identStart     = letter
	, identLetter    = alphaNum
	, opStart        = opLetter gameDataFormatDef
	, opLetter       = oneOf ":/"
	, reservedOpNames= []
	, reservedNames  = []
	, caseSensitive  = True
	}

lexer :: GenTokenParser T.Text u Identity
lexer = makeTokenParser gameDataFormatDef

t_decimal :: ParsecT T.Text u Identity Integer
t_decimal = decimal lexer
-- NOTE: as of Parsec 3.1.5, `hexadecimal` expects numbers with a leading 'x',
-- not a '0x'!
t_hexadecimal :: ParsecT T.Text u Identity Integer
t_hexadecimal = hexadecimal lexer
t_natural :: ParsecT T.Text u Identity Integer
t_natural = natural lexer
t_symbol :: String -> ParsecT T.Text u Identity String
t_symbol = symbol lexer
t_identifier :: ParsecT T.Text u Identity String
t_identifier = identifier lexer
t_whiteSpace :: ParsecT T.Text u Identity ()
t_whiteSpace = whiteSpace lexer
t_braces :: ParsecT T.Text u Identity a -> ParsecT T.Text u Identity a
t_braces = braces lexer
t_brackets :: ParsecT T.Text u Identity a -> ParsecT T.Text u Identity a
t_brackets = brackets lexer
t_stringLiteral :: ParsecT T.Text u Identity String
t_stringLiteral = stringLiteral lexer
t_commaSep1 :: ParsecT T.Text u Identity a -> ParsecT T.Text u Identity [a]
t_commaSep1 = commaSep1 lexer

-- Having a ParsecT type allows us to use this parser in parsers that do or do
-- not involve keeping track of state. If we just have a "Parser Int" type, this
-- parser will not be able to be used inside, e.g., a "GameMapParser a" parser.
intParser :: ParsecT T.Text u Identity Int
intParser = do
	sign <- optionMaybe $ char '-'
	n <- t_decimal
	let
		n' = fromIntegral n
	if isJust sign
		then return $ negate n'
		else return n'

intParser' :: ParsecT T.Text u Identity Int
intParser' = do
	n <- intParser
	_ <- t_whiteSpace
	return n

uintParser :: ParsecT T.Text u Identity Int
uintParser = do
	n <- t_decimal
	let
		n' = fromIntegral n
	return n'

uintParser' :: ParsecT T.Text u Identity Int
uintParser' = do
	n <- uintParser
	_ <- t_whiteSpace
	return n

hexParser :: ParsecT T.Text u Identity Int
hexParser = do
	_ <- char '0'
	n <- t_hexadecimal
	let
		n' = fromIntegral n
	return n'

hexParser' :: ParsecT T.Text u Identity Int
hexParser' = do
	n <- hexParser
	_ <- t_whiteSpace
	return n

intRangeParser :: ParsecT T.Text u Identity (Int, Int)
intRangeParser = try a <|> b
	where
	a = do
		n <- intParser
		_ <- string " "
		m <- intParser
		_ <- t_whiteSpace
		return (n, m)
	b = do
		n <- intParser
		_ <- t_whiteSpace
		return (n, n)

t_stringTillNewline :: ParsecT T.Text u Identity String
t_stringTillNewline = do
	str <- manyTill (noneOf "\n") (try . lookAhead $ trailingWhitespace)
	_ <- t_whiteSpace
	return str
	where
	trailingWhitespace = do
		_ <- many $ oneOf " \t"
		_ <- string "\n"
		return ()

choice' :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice' [] = fail "choice': empty list"
choice' (p:[]) = choice [p]
choice' ps = choice $ tries ++ [lastOne]
	where
	tries = map try (init ps)
	lastOne = last ps

symbolA
	:: String
	-> (ParsecT T.Text u Identity a)
	-> ParsecT T.Text u Identity (SourcePos, a)
symbolA str parser = do
	_ <- t_symbol str
	pos <- getPosition
	a <- parser
	return (pos, a)

symbolWhiteSpace :: String -> ParsecT T.Text u Identity ()
symbolWhiteSpace str = do
	_ <- string str
	_ <- lookAhead $ oneOf " \t\n"
	return ()

sourceLC :: SourcePos -> String
sourceLC sp = paren $ "line "
	++ (show $ sourceLine sp)
	++ ", column " ++ (show $ sourceColumn sp)

duplicateDefinition
	:: (Eq a, Show a, Monad m)
	=> String
	-> [(a, SourcePos)]
	-> (a, SourcePos) -> m b
duplicateDefinition keyType hash (key, pos) = fail
	$ headingBody
		[show pos]
		[ keyType ++ " " ++ show key ++ " already defined at "
			++ (sourceLC . fromJust $ lookup key hash)
		]

valueBeyondRange :: (Eq a, Show a, Monad m) => String -> (a, SourcePos) -> m b
valueBeyondRange valType (val, pos) = fail
	$ headingBody
		[show pos]
		[ valType ++ " " ++ show val ++ " out of range"
		]

headingBody :: [String] -> [String] -> String
headingBody heading body = (intercalate "\n" heading)
	++ "\n"
	++ (intercalate "\n" $ map indent body)

parseErrMsg
	:: (Eq a, Show a, Monad m)
	=> String
	-> String
	-> (String, a, SourcePos)
	-> m b
parseErrMsg msg valLocation (valType, val, pos) = fail
	$ headingBody
		[ ""
		, show pos
		, msg
		]
		[ valLocation
		, valType ++ ": " ++ show val
		]

showTuple :: (Show a, Show b) => (a, b) -> String
showTuple (a, b) = show a ++ " " ++ show b

indent :: String -> String
indent s = "\t" ++ s

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)
