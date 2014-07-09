{-# LANGUAGE RecordWildCards #-}

module NW.State where

import Control.Monad.Primitive
import System.Random.MWC

import NW.Config
import NW.Map
import NW.Monster
import NW.Player

data GameState = GameState
	{ gsGameMap :: GameMap
	, gsPlayer :: Player
	, gsMonsters :: [Monster]
	, gsLastCommand :: String
	, gsLastBattleCommand :: String
	, gsItemDB :: ItemDB
	, gsMonsterDB :: MonsterDB
	, gsRng :: Gen (PrimState IO)
	}
