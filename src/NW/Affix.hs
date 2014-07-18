{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Affix where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Identity
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import NW.Effect
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
	, affixEffects :: [Effect]
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
			return $ Affix
				{ affixClass = affixClass''
				, affixName = affixName'
				, affixEffects = effects
				}

addAffix
	:: (AffixName, SourcePos)
	-> AffixParserState
	-> AffixParserState
addAffix uNamePos aps@AffixParserState{..} = aps
	{ apsAffixNames = uNamePos:apsAffixNames
	}

effectParser :: String -> AffixParser Effect
effectParser affixName' = do
	str <- choice' $ map t_symbol
		[ "health"
		, "mana"
		]
	let
		effectType = case str of
			"health" -> EAttribute Health
			_ -> EAttribute Mana
	posEffect <- getPosition
	n <- numberValParser
	return (effectType, n)

numberValParser :: ParsecT T.Text u Identity NumberVal
numberValParser = choice' $
	[ numberValRangeParser
	, numberValPercParser
	, NVConst <$> intParser'
	]

numberValPercParser :: ParsecT T.Text u Identity NumberVal
numberValPercParser = do
	pos <- getPosition
	n <- intParser
	_ <- t_symbol "p"
	return $ NVPerc n

numberValRangeParser :: ParsecT T.Text u Identity NumberVal
numberValRangeParser = do
	posRange <- getPosition
	range@(a, b) <- intRangeParser
	if
		| a > b -> valueBeyondRange "range" (a, posRange)
		| a < 2 -> valueBeyondRange "range" (a, posRange)
		| otherwise -> return $ NVRange range
