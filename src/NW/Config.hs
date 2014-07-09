{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Config where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson.Types
import qualified Data.Vector as V

import NW.Item
import NW.Monster

type MonsterDB = V.Vector Monster
type ItemDB = V.Vector Item

data Config = Config
	{ cfgMap :: FilePath
	, cfgMonsters :: FilePath
	, cfgItems :: FilePath
	} deriving (Eq, Show)

instance FromJSON Config where
	parseJSON (Object o) = Config
		<$> o .: "map"
		<*> o .: "monster-db"
		<*> o .: "item-db"
	parseJSON v = typeMismatch "Config" v
