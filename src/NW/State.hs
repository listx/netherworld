{-# LANGUAGE RecordWildCards #-}

module NW.State where

import Control.Monad.Primitive
import Data.Word
import System.Random.MWC

import NW.Affix
import NW.Config
import NW.Map
import NW.Monster
import NW.Player

data GameState = GameState
	{ gsConfig :: Config
	, gsGameMap :: GameMap
	, gsPlayer :: Player
	, gsMonsters :: [Monster]
	, gsLastCommand :: String
	, gsLastBattleCommand :: String
	, gsAffixDB :: AffixDB
	, gsMonsterDB :: MonsterDB
	, gsRngInitialState :: [Word32]
	, gsRng :: Gen (PrimState IO)
	, gsInputHistory :: [String]
	, gsReplay :: Bool
	}

getUserInput :: GameState -> IO (GameState, String)
getUserInput gs@GameState{..}
	| gsReplay = do
		return (gs {gsInputHistory = tail gsInputHistory}, head gsInputHistory)
	| otherwise = do
		str <- getLine
		putStrLn $ "INPUT IS: " ++ show str
		let
			gs1 = gs
				{ gsInputHistory = str : gsInputHistory
				}
		return (gs1, str)
