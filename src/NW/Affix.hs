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
import System.Random.MWC
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import NW.Effect
import NW.Error
import NW.Stats
import NW.Util

data AffixClass
	= ACAdj -- "Icy" or "Incinerating"
	| ACNoun -- "of Lamentation"
	| ACNounProper -- "of the Bear"
	| ACPersona -- "Killer's" or "Assassin's"
	| ACName -- "Daniel's" or "of Achilles"
	deriving (Eq, Enum, Show)

instance Variate AffixClass where
	uniform rng = return . toEnum =<< uniformR (fromEnum ACAdj, fromEnum ACName) rng
	uniformR _ _ = error "uniformR: Accessory unsupported"

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
	affixClass' <- choice' $ map (\(a, b) -> t_symbol a >> return b)
		[ ("adj", ACAdj)
		, ("noun-proper", ACNounProper)
		, ("noun", ACNoun)
		, ("persona", ACPersona)
		, ("name", ACName)
		]

	posAffixName <- getPosition
	affixName' <- t_stringLiteral

	effectss <- many1 effectsParser

	if
		| elem affixName' (map fst apsAffixNames)
			-> duplicateDefinition "affix" apsAffixNames (affixName', posAffixName)
		| otherwise -> do
			modifyState $ addAffix (affixName', posAffixName)
			return $ Affix
				{ affixClass = affixClass'
				, affixName = affixName'
				, affixEffects = concat $ effectss
				}

addAffix
	:: (AffixName, SourcePos)
	-> AffixParserState
	-> AffixParserState
addAffix uNamePos aps@AffixParserState{..} = aps
	{ apsAffixNames = uNamePos:apsAffixNames
	}

effectsParser :: AffixParser [Effect]
effectsParser = do
	effectTypes <- choice' $ map (\(a, b) -> t_symbol a >> return b)
		[ ("health", [EAttribute Health])
		, ("mana", [EAttribute Mana])
		, ("strength", [EAttribute Strength])
		, ("wisdom", [EAttribute Wisdom])
		, ("attack", [EAttribute Attack])
		, ("magic-attack", [EAttribute MAttack])
		, ("defense", [EAttribute Defense])
		, ("magic-defense", [EAttribute MDefense])
		, ("damage-earth", [EAttribute (Damage Earth)])
		, ("damage-fire", [EAttribute (Damage Fire)])
		, ("damage-cold", [EAttribute (Damage Cold)])
		, ("damage-lightning", [EAttribute (Damage Lightning)])
		,
			( "damage-all"
			,
				[ EAttribute (Damage Earth)
				, EAttribute (Damage Fire)
				, EAttribute (Damage Cold)
				, EAttribute (Damage Lightning)
				]
			)
		, ("resist-earth", [EAttribute (Resist Lightning)])
		, ("resist-fire", [EAttribute (Resist Lightning)])
		, ("resist-cold", [EAttribute (Resist Lightning)])
		, ("resist-lightning", [EAttribute (Resist Lightning)])
		,
			( "resist-all"
			,
				[ EAttribute (Resist Earth)
				, EAttribute (Resist Fire)
				, EAttribute (Resist Cold)
				, EAttribute (Resist Lightning)
				]
			)
		, ("lifesteal", [EAttribute LifeSteal])
		, ("magic-item-find", [EGameMechanics MagicItemFind])
		, ("gold-earned", [EGameMechanics GoldEarned])
		]
	n <- numberValParser
	return . zip effectTypes $ repeat n

numberValParser :: ParsecT T.Text u Identity NumberVal
numberValParser = choice' $
	[ numberValPercParser
	, numberValRangeParser
	, NVConst <$> intParser'
	]

numberValPercParser :: ParsecT T.Text u Identity NumberVal
numberValPercParser = do
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
