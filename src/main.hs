{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment

import NW.Map
import NW.Player

data GameState = GameState
	{ gsGameMap :: GameMap
	, gsPlayer :: Player
	, gsLastCommand :: String
	}

main :: IO ()
main = do
	args <- getArgs
	gameMap <- importMap $ head args
	let
		gs = GameState
			{ gsGameMap = gameMap
			, gsPlayer = player
			, gsLastCommand = ""
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
	case str of
		"quit" -> return ()
		"q" -> return ()
		"e" -> goIfOk str
		"w" -> goIfOk str
		"n" -> goIfOk str
		"s" -> goIfOk str
		"ne" -> goIfOk str
		"nw" -> goIfOk str
		"se" -> goIfOk str
		"sw" -> goIfOk str
		"" -> goIfOk gsLastCommand
		_ -> do
			putStrLn "You stall in confusion."
			gameLoop gs
	where
	m@GameMap{..} = gsGameMap
	p@Player{..} = gsPlayer
	goIfOk :: String -> IO ()
	goIfOk str
		| inRange m c = case midx gameMapVector c of
			Just _ -> gameLoop gs
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
