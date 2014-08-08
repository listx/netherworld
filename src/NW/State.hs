{-# LANGUAGE RecordWildCards #-}

module NW.State where

import Control.Monad.Primitive
import qualified Data.Vector as V
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
	, gsRngInitial :: [Word32]
	, gsRng :: Gen (PrimState IO)
	, gsInputHistory :: [String]
	, gsReplay :: Bool
	, gsDebug :: Bool
	}

gsDefault :: GameState
gsDefault = GameState
	{ gsConfig = configDefault
	, gsGameMap = GameMap
		{ gameMapVector = V.empty
		, gameMapRange = (0, 0)
		}
	, gsPlayer = Player
		{ playerCoord = (0, 0)
		, playerStats = []
		}
	, gsMonsters = []
	, gsLastCommand = ""
	, gsLastBattleCommand = ""
	, gsAffixDB = []
	, gsMonsterDB = []
	, gsRngInitial = []
	, gsRng = undefined
	, gsInputHistory = []
	, gsReplay = False
	, gsDebug = False
	}

getUserInput :: GameState -> IO (GameState, String)
getUserInput gs@GameState{..}
	| gsReplay = do
		putStrLn $ "getUserInput: replaying input `" ++ head gsInputHistory ++ "'"
		return (gs {gsInputHistory = tail gsInputHistory}, head gsInputHistory)
	| otherwise = do
		str <- getLine
		let
			gs1 = gs
				{ gsInputHistory = str : gsInputHistory
				}
		return (gs1, str)

nwPuts :: GameState -> String -> IO ()
nwPuts GameState{..} str = if not gsReplay || gsDebug
	then putStrLn str
	else return ()
