{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Config where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.FilePath
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import NW.Error
import NW.Util

data Config = Config
	{ cfgMap :: FilePath
	, cfgAffixDB :: FilePath
	, cfgMonsterDB :: FilePath
	} deriving (Eq, Show)

configDefault :: Config
configDefault = Config
	{ cfgMap = ""
	, cfgAffixDB = ""
	, cfgMonsterDB = ""
	}

importConfig :: FilePath -> IO (Either Int Config)
importConfig fname = do
	fsrc <- T.readFile fname
	parseConfig fname fsrc

parseConfig :: String -> T.Text -> IO (Either Int Config)
parseConfig fname src = case parse configParser fname src of
	Left parseError -> do
		errMsg "parseConfig: could not parse file "
		hPutStrLn stderr (show parseError)
		return $ Left 3
	Right result -> return $ Right result

configParser :: Parser Config
configParser = do
	_ <- t_whiteSpace
	_ <- t_symbol "map"
	m <- t_stringLiteral

	_ <- t_symbol "affix-db"
	affixDb <- t_stringLiteral

	_ <- t_symbol "monster-db"
	monsterDb <- t_stringLiteral
	return $ Config
		{ cfgMap = m
		, cfgAffixDB = affixDb
		, cfgMonsterDB = monsterDb
		}

sanitizeConfig :: FilePath -> Config -> Config
sanitizeConfig dir c@Config{..} = c
	{ cfgMap = combine dir cfgMap
	, cfgAffixDB = combine dir cfgAffixDB
	, cfgMonsterDB = combine dir cfgMonsterDB
	}
