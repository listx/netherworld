{-# LANGUAGE RecordWildCards #-}

module NW.Stats where

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

modStat :: Attribute -> (Int -> Int) -> [Stat] -> [Stat]
modStat attr f stats = case lookup attr stats of
	Just n -> let
		stats' = filter ((/=attr) . fst) stats
		in
		(attr, f n):stats'
	Nothing -> stats
