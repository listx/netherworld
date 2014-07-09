{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Item where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson.Types
import qualified Data.Text as T

import NW.Stats
import NW.Util

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

instance FromJSON ItemClass where
	parseJSON (String t) = case t of
		"armor" -> pure (IC GArmor)
		"weapon" -> pure (IC GWeapon)
		"accessory" -> pure (IC GAccessory)
		"potion" -> pure ICPotion
		x -> fail $ "unknown identifier " ++ squote (T.unpack x) ++ " for type ItemClass"
	parseJSON v = typeMismatch "ItemClass" v

data Item = Item
	{ itemClass :: ItemClass
	, itemName :: T.Text
	, itemStatMods :: [Stat]
	, itemBattleStatMods :: [BattleStat]
	} deriving (Eq, Show)

instance FromJSON Item where
	parseJSON (Object o) = Item
		<$> o .: "class"
		<*> o .: "name"
		<*> o .: "stats-mods"
		<*> o .: "bstats-mods"
	parseJSON v = typeMismatch "Item" v
