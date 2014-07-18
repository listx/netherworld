{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Affix where

import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import NW.Error
import NW.Stats
import NW.Util

data AffixClass
	= AffixAdj
	| AffixName
	deriving (Eq, Show)

type AffixName = String

data Affix = Affix
	{ affixClass :: AffixClass
	, affixName :: AffixName
	, affixEffects :: [StatRange]
	} deriving (Eq, Show)

type AffixDB = [Affix]

importGMAffixes :: FilePath -> IO [Affix]
importGMAffixes fname = do
	fsrc <- T.readFile fname
	(parseSuccess, result) <- parseGMAffixes fname fsrc
	when (not parseSuccess) $ exitFailure
	return result

parseGMAffixes :: String -> T.Text -> IO (Bool, [Affix])
parseGMAffixes fname src = case runP affixDBParser cleanAPS fname src of
	Left parseError -> do
		errMsg "parseGMAffixes: could not parse file "
		hPutStrLn stderr (show parseError)
		return (False, [])
	Right result -> return (True, result)
	where
	cleanAPS :: AffixParserState
	cleanAPS = AffixParserState
		{ apsAffixNames = []
		}

type AffixParser = GenParser AffixParserState
data AffixParserState = AffixParserState
	{ apsAffixNames :: [(AffixName, SourcePos)]
	}

affixDBParser :: AffixParser [Affix]
affixDBParser = do
	_ <- t_whiteSpace
	affixes <- many1 affixParser
	eof
	return affixes

affixParser :: AffixParser Affix
affixParser = do
	AffixParserState{..} <- getState
	_ <- t_symbol "affix"
	affixClass' <- choice' $ map t_symbol
		[ "adj"
		, "name"
		]

	posAffixName <- getPosition
	affixName' <- t_stringLiteral

	effects <- many1 $ effectParser affixName'

	if
		| elem affixName' (map fst apsAffixNames)
			-> duplicateDefinition "affix" apsAffixNames (affixName', posAffixName)
		| otherwise -> do
			modifyState $ addAffix (affixName', posAffixName)
			let
				affixClass'' = case affixClass' of
					"adj" -> AffixAdj
					_ -> AffixName
				effs = getStatRanges effects
			return $ Affix
				{ affixClass = affixClass''
				, affixName = affixName'
				, affixEffects = effs
				}

addAffix
	:: (AffixName, SourcePos)
	-> AffixParserState
	-> AffixParserState
addAffix uNamePos aps@AffixParserState{..} = aps
	{ apsAffixNames = uNamePos:apsAffixNames
	}

effectParser :: String -> AffixParser (String, (Int, Int))
effectParser affixName' = do
	str <- choice' $ map t_symbol
		[ "health"
		, "mana"
		]
	posEffect <- getPosition
	range@(a, b) <- intRangeParser
	if
		| a > b -> valueBeyondRange (affixName' ++ ": " ++ str) (a, posEffect)
		| a < 2 -> valueBeyondRange (affixName' ++ ": " ++ str) (a, posEffect)
		| otherwise -> return (str, range)

getStatRanges :: [(String, (Int, Int))] -> [StatRange]
getStatRanges = map toStatRange

toStatRange :: (String, (Int, Int)) -> StatRange
toStatRange (str, range) = (stat, range)
	where
	stat = case str of
		"health" -> Health
		"mana" -> Health
		_ -> error $ "unknown attribute: " ++ str
