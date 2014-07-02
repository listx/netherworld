{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Primitive
import System.Environment
import System.Random.MWC

import NW.Map
import NW.Player
import NW.Random

data GameState = GameState
	{ gsGameMap :: GameMap
	, gsPlayer :: Player
	, gsLastCommand :: String
	, gsRng :: Gen (PrimState IO)
	}

main :: IO ()
main = do
	args <- getArgs
	gameMap <- importMap $ head args
	rng <- mkGen $ SeedRandom
	let
		gs = GameState
			{ gsGameMap = gameMap
			, gsPlayer = player
			, gsLastCommand = ""
			, gsRng = rng
			}
	gameLoop gs
	where
	player = Player
		{ playerCoord = (0, 0)
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
				roll <- uniformR ((0, 100)::(Int, Int)) gsRng
				if (roll < 7)
					then putStrLn "You enter a battle!!!"
					else return ()
				gameLoop gs
					{ gsGameMap = m
					, gsPlayer = p {playerCoord = c}
					, gsLastCommand = str
					}
			Nothing -> do
				putStrLn "You cannot go there."
				gameLoop gs
		| otherwise = do
			putStrLn "You cannot go there."
			gameLoop gs
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
