{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Battle where

import NW.Monster
import NW.Random
import NW.State
import NW.Stats

spawnMonsters :: GameState -> IO GameState
spawnMonsters gs@GameState{..} = do
	m <- getMonster
	return gs
		{ gsMonsters = [m]
		}
	where
	getMonster = do
		r <- roll 100 gsRng
		return $ Monster
			{ mClass = MCFighter
			, mName = "monster"
			, mStatsBase = modStat Health (\_ -> r) $ statsBaseDefault
			, mLootBonus = 0
			}

battleTrigger :: GameState -> IO GameState
battleTrigger gs@GameState{..} = do
	r <- roll 100 gsRng
	if (r < 7)
		then do
			nwPuts gs "You enter a battle!!!"
			gs1 <- spawnMonsters gs
			gs2 <- battleLoop gs1
			return gs2
		else return gs

battleLoop :: GameState -> IO GameState
battleLoop gs0 = do
	gs1 <- battlePlayerOption gs0
	gs2 <- battleMonsterOption gs1
	if
		| null (gsMonsters gs2) -> do
			nwPuts gs2 "You defeat all monsters!"
			return gs2
		| otherwise -> battleLoop gs2

battlePlayerOption :: GameState -> IO GameState
battlePlayerOption gs = do
	(gs1, str) <- getUserInput gs
	let
		tokens = words str
		str1 = if length tokens > 0
			then str
			else gsLastBattleCommand gs1
	case str1 of
		"f" -> do
			r <- roll 100 $ gsRng gs1
			let
				gsMonsters1 = map (modMonsterStat Health (\n -> n - r)) $ gsMonsters gs1
				gsMonsters2 = filter monsterAlive gsMonsters1
			nwPuts gs1 $ "You do " ++ show r ++ " damage!"
			if null gsMonsters2
				then return gs1
					{ gsMonsters = []
					, gsLastBattleCommand = ""
					}
				else return gs1
					{ gsMonsters = gsMonsters1
					, gsLastBattleCommand = str1
					}
		_ -> do
			nwPuts gs1 "What?"
			battlePlayerOption gs1
				{ gsLastBattleCommand = str1
				}
	where
	modMonsterStat :: Attribute -> (Int -> Int) -> Monster -> Monster
	modMonsterStat attr f m@Monster{..} = m
		{ mStatsBase = modStat attr f mStatsBase
		}
	monsterAlive :: Monster -> Bool
	monsterAlive Monster{..} = case lookup Health mStatsBase of
		Just n -> n > 0
		Nothing -> True -- if monster is a non-health creature, it's always alive... like ghosts!

battleMonsterOption :: GameState -> IO GameState
battleMonsterOption gs@GameState{..} = do
	return gs
