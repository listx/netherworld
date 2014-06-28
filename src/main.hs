{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map.Lazy as M
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
	putStrLn $ miniMapView m 10 playerCoord
	putStrLn $ show playerCoord
	str <- getLine
	case str of
		"quit" -> return ()
		"q" -> return ()
		"e" -> goIfOk East
		"w" -> goIfOk West
		"n" -> goIfOk North
		"s" -> goIfOk South
		_ -> gameLoop p m
	where
	goIfOk :: Direction -> IO ()
	goIfOk d = if M.member c gameMap
		then gameLoop p {playerCoord = c} m
		else do
			putStrLn "cannot go there"
			gameLoop p m
		where
		c = goDir playerCoord d
