{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Monster where

import NW.Stats

data MonsterClass
	= MCFighter
	| MCMage
	deriving (Eq, Show)

data Monster = Monster
	{ mClass :: MonsterClass
	, mName :: String
	, mStatsBase :: [Stat]
	, mLootBonus :: Int
	} deriving (Eq, Show)

type MonsterDB = [Monster]
