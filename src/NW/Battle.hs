{-# LANGUAGE MultiWayIf #-}
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
			{ mStatsBase = modStat Health (\_ -> r) $ statsBaseDefault
			}

battleLoop :: GameState -> IO GameState
battleLoop gs0 = do
	gs1 <- battlePlayerOption gs0
	gs2 <- battleMonsterOption gs1
	if
		| null (gsMonsters gs2) -> do
			putStrLn "You defeat all monsters!"
			return gs2
		| otherwise -> battleLoop gs2

battlePlayerOption :: GameState -> IO GameState
battlePlayerOption gs@GameState{..} = do
	str <- getLine
	if length (words str) > 0
		then runOption str
		else runOption gsLastCommand
	where
	runOption :: String -> IO GameState
	runOption str = case str of
		"f" -> do
			r <- roll 100 gsRng
			let
				gsMonsters1 = map (modMonsterStat Health (\n -> n - r)) gsMonsters
				gsMonsters2 = filter monsterAlive gsMonsters1
			putStrLn $ "You do " ++ show r ++ " damage!"
			if null gsMonsters2
				then return gs
					{ gsMonsters = []
					, gsLastCommand = str
					}
				else return gs
					{ gsMonsters = gsMonsters1
					, gsLastCommand = str
					}
		_ -> do
			putStrLn "What?"
			battlePlayerOption gs
		where
--		tokens = words str
--		comHead = head tokens
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
