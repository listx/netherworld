{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Vector as V
import System.Environment

import NW.Battle
import NW.Map
import NW.Player
import NW.Random
import NW.State
import NW.Stats

main :: IO ()
main = do
	args <- getArgs
	gameMap <- importMap $ head args
	rng <- mkGen $ SeedRandom
	let
		gs = GameState
			{ gsGameMap = gameMap
			, gsPlayer = player gameMap
			, gsMonsters = []
			, gsLastCommand = ""
			, gsLastBattleCommand = ""
			, gsItemDB = V.empty
			, gsMonsterDB = V.empty
			, gsRng = rng
			}
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
