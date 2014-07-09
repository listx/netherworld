{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Config where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Yaml
import System.FilePath

import NW.Item
import NW.Monster

type MonsterDB = V.Vector Monster
type ItemDB = V.Vector Item

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

importConfig :: FilePath -> IO (Maybe Config)
importConfig fp = do
	x <- decodeFileEither fp
	case x of
		Left e -> do
			putStrLn $ show e
			return Nothing
		Right cfg -> return $ Just cfg

sanitizeConfig :: FilePath -> Config -> Config
sanitizeConfig dir c@Config{..} = c
	{ cfgMap = combine dir cfgMap
	, cfgItemDB = combine dir cfgItemDB
	, cfgMonsterDB = combine dir cfgMonsterDB
	}
