{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Effect where

import NW.Stats

data EffectType
	= EAttribute Attribute
	| EGameMechanics GameMechanics
	deriving (Eq, Show)

data GameMechanics
	= MagicItemFind
	| GoldEarned
	deriving (Eq, Show)

type Effect = (EffectType, NumberVal)

data NumberVal
	= NVConst Int
	| NVPerc Int
	| NVRange (Int, Int)
	deriving (Eq, Show)
