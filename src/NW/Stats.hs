{-# LANGUAGE RecordWildCards #-}

module NW.Stats where

--data StatsBase = StatsBase
--	{ health :: Int
--	, mana :: Int
--	, strength :: Int
--	, wisdom :: Int -- rate at which u replenish mana
--	}

data Attribute
	= Health
	| Mana
	| Strength
	| Wisdom
	deriving (Eq, Show)

type Stat = (Attribute, Int)

statsBaseDefault :: [Stat]
statsBaseDefault =
	[ (Health, 100)
	, (Mana, 100)
	, (Strength, 10)
	, (Wisdom, 10)
	]

--statsBaseDefault :: StatsBase
--statsBaseDefault = StatsBase
--	{ health = 100
--	, mana = 100
--	, strength = 10
--	, wisdom = 10
--	}

modStat :: Attribute -> (Int -> Int) -> [Stat] -> [Stat]
modStat attr f stats = case lookup attr stats of
	Just n -> let
		stats' = filter ((/=attr) . fst) stats
		in
		(attr, f n):stats'
	Nothing -> stats
