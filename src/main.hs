{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment

import NW.Map
import NW.Player

main :: IO ()
main = do
	args <- getArgs
	gameMap <- importMap $ head args
	gameLoop player gameMap
	where
	player = Player
		{ playerCoord = (0, 0)
		}

gameLoop :: Player -> Map -> IO ()
gameLoop player gameMap = do
	putStrLn $ cameraRegion gameMap player
	putStrLn $ show (playerCoord player)
	str <- getLine
	case str of
		"quit" -> return ()
		"q" -> return ()
		"e" -> gameLoop (goDir East (mapSize gameMap) player) gameMap
		"w" -> gameLoop (goDir West (mapSize gameMap) player) gameMap
		"n" -> gameLoop (goDir North (mapSize gameMap) player) gameMap
		"s" -> gameLoop (goDir South (mapSize gameMap) player) gameMap
		_ -> gameLoop player gameMap
