{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Stats where

-- Stats are *finalized* values after all spells/effects/conditions etc. are
-- applied to the character.

data Attribute
	= Health
	| Mana
	| Strength -- how much damage a physical attack does
	| Wisdom -- how much damage a spell does
	| Attack -- chance to hit enemy
	| MAttack -- chance for spell to affect enemy
	| Defense -- chance to evade a hit
	| MDefense -- chance to evade a spell
	| Damage -- how much base damage an item does
	| DamageE Element
	| Resist Element
	| LifeSteal
	deriving (Eq, Show)

type Stat = (Attribute, Int)
type StatRange = (Attribute, (Int, Int))

data Element
	= Earth
	| Fire
	| Cold
	| Lightning
	| ElementAll
	deriving (Eq, Enum, Show)

statsBaseDefault :: [Stat]
statsBaseDefault =
	[ (Health, 100)
	, (Mana, 100)
	, (Strength, 10)
	, (Wisdom, 10)
	]

modStat :: Attribute -> (Int -> Int) -> [Stat] -> [Stat]
modStat attr f stats = case lookup attr stats of
	Just n -> (attr, f n):stats'
		where
		stats' = filter ((/=attr) . fst) stats
	Nothing -> stats
