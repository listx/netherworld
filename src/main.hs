{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe
import System.FilePath
import System.Environment

import NW.Battle
import NW.Config
import NW.Item
import NW.Error
import NW.Map
--import NW.Monster
import NW.Player
import NW.Random
import NW.State
import NW.Stats
--import NW.Util

main :: IO ()
main = do
	args <- getArgs
	let
		cfg = head args
	x <- importConfig cfg
	config <- case x of
		Left _ -> do
			return Nothing
		Right result -> return $ Just result
	failIfNothing config "Config"
	let
		config' = sanitizeConfig (takeDirectory cfg) $ fromJust config
	gameMap <- importMap $ cfgMap config'
--	affixDB <- decodeFileEither' $ cfgAffixDB config' :: IO (Maybe AffixDB)
--	failIfNothing affixDB "AffixDB"
--	monsterDB <- decodeFileEither' $ cfgMonsterDB config' :: IO (Maybe MonsterDB)
--	failIfNothing monsterDB "MonsterDB"
	rng <- mkGen $ SeedRandom
	let
--		affixDB' = fromJust affixDB
--		monsterDB' = fromJust monsterDB
		gs = GameState
			{ gsGameMap = gameMap
			, gsPlayer = player gameMap
			, gsMonsters = []
			, gsLastCommand = ""
			, gsLastBattleCommand = ""
			, gsAffixDB = []
			, gsMonsterDB = []
			, gsRng = rng
			}
--	pshow affixDB'
--	pshow monsterDB'
	gameLoop gs
	where
	player gm = Player
		{ playerCoord = firstCoord gm
		, playerStats = statsBaseDefault
		}

gameLoop :: GameState -> IO ()
gameLoop gs@GameState{..} = do
	putStrLn $ miniMapView m playerCoord
	putStrLn $ show playerCoord
	str <- getLine
	let
		tokens = words str
	if length tokens > 0
		then if
			| elem (head tokens) directionals -> goIfOk str
			| otherwise -> case head tokens of
				"quit" -> return ()
				"q" -> return ()
				_ -> do
					putStrLn "You stall in confusion."
					gameLoop gs
		else goIfOk gsLastCommand
	where
	directionals = ["e", "w", "n", "s", "ne", "nw", "se", "sw"]
	m@GameMap{..} = gsGameMap
	p@Player{..} = gsPlayer
	goIfOk :: String -> IO ()
	goIfOk str
		| inRange m c = case midx gameMapVector c of
			Just _ -> do
				let
					gs1 = gs
						{ gsGameMap = m
						, gsPlayer = p {playerCoord = c}
						, gsLastCommand = str
						}
				r <- roll 100 gsRng
				if (r < 7)
					then do
						putStrLn "You enter a battle!!!"
						gs2 <- spawnMonsters gs1
						gs3 <- battleLoop gs2
						gameLoop gs3
					else gameLoop gs1
			Nothing -> do
				putStrLn "You cannot go there."
				gameLoop gs
					{ gsLastCommand = str
					}
		| otherwise = do
			putStrLn "You cannot go there."
			gameLoop gs
				{ gsLastCommand = str
				}
		where
		d = case str of
			"e" -> [East]
			"w" -> [West]
			"n" -> [North]
			"s" -> [South]
			"ne" -> [North, East]
			"nw" -> [North, West]
			"se" -> [South, East]
			_ -> [South, West]
		c = foldl goDir playerCoord d
