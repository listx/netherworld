{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Config where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson.Types
import System.FilePath

data Config = Config
	{ cfgMap :: FilePath
	, cfgItemDB :: FilePath
	, cfgMonsterDB :: FilePath
	} deriving (Eq, Show)

instance FromJSON Config where
	parseJSON (Object o) = Config
		<$> o .: "map"
		<*> o .: "item-db"
		<*> o .: "monster-db"
	parseJSON v = typeMismatch "Config" v

sanitizeConfig :: FilePath -> Config -> Config
sanitizeConfig dir c@Config{..} = c
	{ cfgMap = combine dir cfgMap
	, cfgItemDB = combine dir cfgItemDB
	, cfgMonsterDB = combine dir cfgMonsterDB
	}
