{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Stats where

import Control.Applicative ((<$>), pure)
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import NW.Util

data Attribute
	= Health
	| Mana
	| Strength
	| Wisdom
	deriving (Eq, Enum, Show)

type Stat = (Attribute, Int)

instance FromJSON Attribute where
	parseJSON (String t) = case lookup t' h of
		Just a -> pure a
		Nothing -> fail' t' (map fst h)
		where
		t' = T.unpack t
		h = enumsHash $ enumFrom Health
	parseJSON v = typeMismatch "Attribute" v

data BattleAttribute
	= Attack
	| MAttack
	| Defense
	| MDefense
	| Resist Element
	deriving (Eq, Show)

type BattleStat = (BattleAttribute, Int)

instance FromJSON BattleAttribute where
	parseJSON (String t) = case t' of
		"attack" -> pure Attack
		"mattack" -> pure MAttack
		"defense" -> pure Defense
		"mdefense" -> pure MDefense
		_ -> fail "BattleAttribute: unexpected format"
		where
		t' = T.unpack t
	parseJSON (Object o) = case H.toList o of
		[("resist", o'@(Object _))] -> Resist <$> parseJSON o'
		_ -> fail "BattleAttribute: unexpected format"
	parseJSON v = typeMismatch "BattleAttribute" v

data Element
	= Earth
	| Fire
	| Ice
	| Lightning
	deriving (Eq, Enum, Show)

instance FromJSON Element where
	parseJSON (String t) = case lookup t' h of
		Just a -> pure a
		Nothing -> fail' t' (map fst h)
		where
		t' = T.unpack t
		h = enumsHash $ enumFrom Earth
	parseJSON v = typeMismatch "Element" v

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
