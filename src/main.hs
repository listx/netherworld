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

gameLoop :: Player -> GameMap -> IO ()
gameLoop p@Player{..} m@GameMap{..} = do
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
			gameLoop p m
	where
	goIfOk :: Direction -> IO ()
	goIfOk d
		| inRange m c = case midx gameMapVector c of
			Just _ -> gameLoop p {playerCoord = c} m
			Nothing -> do
				putStrLn "You cannot go there."
				gameLoop p m
		| otherwise = do
			putStrLn "You cannot go there."
			gameLoop p m
		where
		c = goDir playerCoord d
