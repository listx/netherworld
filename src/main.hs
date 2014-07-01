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
		"e" -> goIfOk East
		"w" -> goIfOk West
		"n" -> goIfOk North
		"s" -> goIfOk South
		_ -> do
			putStrLn "You stall in confusion."
			gameLoop gs
	where
	m@GameMap{..} = gsGameMap
	p@Player{..} = gsPlayer
	goIfOk :: Direction -> IO ()
	goIfOk d
		| inRange m c = case midx gameMapVector c of
			Just _ -> gameLoop gs
				{ gsGameMap = m
				, gsPlayer = p {playerCoord = c}
				}
			Nothing -> do
				putStrLn "You cannot go there."
				gameLoop gs
		| otherwise = do
			putStrLn "You cannot go there."
			gameLoop gs
		where
		c = goDir playerCoord d
