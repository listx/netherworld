{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import System.Environment

type Coord = (Int, Int)

importMap :: FilePath -> IO Map
importMap fp = do
	f <- readFile fp
	let
		sizeX = length ((lines f)!!0)
	return $ Map
		{ rawStrs = lines f
		}
	where
	padding = replicate padAmt ' '
	paddingTop n = replicate padAmt (replicate (n + padAmt) ' ')
	addPaddingLR line = padding ++ line ++ padding

padAmt :: Int
padAmt = 10

cameraRegion :: Map -> Player -> String
cameraRegion m@Map{..} p@Player{..} = unlines $ regionTop ++ middle ++ regionBottom
	where
	(x, y) = playerCoord
	playerLine' = playerLine p m
	(xMax, yMax) = mapSize m
	middle = [buildLeft ++ "@" ++ buildRight]
	regionTop = map buildLine [(y-padAmt)..(y-1)]
	regionBottom = map buildLine [(y+1)..(y+padAmt)]
	buildLeft = reverse $ foldl searchRange [] [(x-padAmt)..(x-1)]
	buildRight = reverse $ foldl searchRange [] [(x+1)..(x+padAmt)]
	buildLine y = reverse $ foldl searchRange2 [] [(x-padAmt)..(x+padAmt)]
		where
		searchRange2 acc idx
			| idx >= 0 && idx < xMax && y >= 0 && y < yMax = (rawStrs!!y)!!idx:acc
			| otherwise = borderChar:acc
	searchRange acc idx
		| idx >= 0 && idx < xMax = playerLine'!!idx:acc
		| otherwise = borderChar:acc
	borderChar = toEnum 0x25a0

playerLine :: Player -> Map -> String
playerLine Player{..} Map{..} = rawStrs!!(snd playerCoord)

mapSize :: Map -> Coord
mapSize Map{..} = (x, y)
	where
	x = length $ rawStrs!!0
	y = length rawStrs

data Player = Player
	{ playerCoord :: Coord
	}

data Map = Map
	{ rawStrs :: [String]
	} deriving (Eq, Show)

data Direction
	= East
	| West
	| North
	| South
	deriving (Eq, Show)

goDir :: Direction -> Coord -> Player -> Player
goDir d (xMax, yMax) p@Player{..} = p { playerCoord = case d of
	East -> if (x + 1) < xMax
		then (x + 1, y)
		else (x, y)
	West -> if (x - 1) >= 0
		then (x - 1, y)
		else (x, y)
	North -> if (y - 1) >= 0
		then (x, y - 1)
		else (x, y)
	South -> if (y + 1) < yMax
		then (x, y + 1)
		else (x, y)
	}
	where
	(x, y) = playerCoord

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

