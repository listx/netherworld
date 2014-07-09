{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Monster where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson.Types
import qualified Data.Text as T

import NW.Stats
import NW.Util

data MonsterClass
	= MCFighter
	| MCMage
	deriving (Eq, Show)

instance FromJSON MonsterClass where
	parseJSON (String t) = case t of
		"fighter" -> pure MCFighter
		"mage" -> pure MCMage
		x -> fail $ "unknown identifier " ++ squote (T.unpack x) ++ " for type MonsterClass"
	parseJSON v = typeMismatch "MonsterClass" v

data Monster = Monster
	{ mClass :: MonsterClass
	, mName :: T.Text
	, mStatsBase :: [Stat]
	, mLootBonus :: Int
	} deriving (Eq, Show)

instance FromJSON Monster where
	parseJSON (Object o) = Monster
		<$> o .: "class"
		<*> o .: "name"
		<*> o .: "stats"
		<*> o .: "loot-bonus"
	parseJSON v = typeMismatch "Monster" v
