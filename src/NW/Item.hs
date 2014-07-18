{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Item where

import NW.Effect
import NW.Random
import NW.Stats

data ItemClass
	= IC Gear -- something you wear (armor, weapons)
	| ICPotion -- something you drink to heal/get temporary effects
--	| Scroll -- one-shot magical spells
	deriving (Eq, Show)

data Gear
	= GArmor
	| GWeapon
	| GAccessory -- pendants, amulets, rings, necklaces, etc.
	deriving (Eq, Show)
