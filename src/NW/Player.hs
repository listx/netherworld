{-# LANGUAGE RecordWildCards #-}

module NW.Player where

import Data.List

import NW.Map
import NW.Stats

data Player = Player
	{ playerCoord :: Coord
	, playerStats :: [Stat]
	} deriving (Eq, Show)

data Direction
	= East
	| West
	| North
	| South
	deriving (Eq, Show)

-- | Imagine the traditional Cartesian coordinates; we draw a square that is (n
-- * 2 + 1) big on all sides, centered around the origin (0, 0), and we grab all
-- the integer coordinates that fall inside this square. This is the set of
-- offset coordinates (think of them as coordinates relative to the Player). We
-- then modify these coordinates to their equivalents on the actual game map
-- itself (at 'applyOffset'), and then convert each coordinate to the
-- corresponding game map's tile. The way in which the final string is formed
-- depends entirely on how the offset coordinates are first generated (see
-- 'westernEdge').
miniMapView :: GameMap -> Coord -> String
miniMapView m@GameMap{..} coordCenter@(xCenter, yCenter)
	= intercalate "\n"
	$ map (map (coordToChar . applyOffset) . genToEasternEdge) westernEdge
	where
	n = padAmt
	miniMapLen = n * 2 + 1
	westernEdge :: [(Int, Int)]
	westernEdge = zip (replicate miniMapLen (-n)) [n,(n-1)..]
	genToEasternEdge (x, y) = [(x', y) | x' <- take miniMapLen [x..]]
	coordToChar :: (Int, Int) -> Char
	coordToChar c
		| not (inRange m c) = 'X'
		| c /= coordCenter = getTile $ midx gameMapVector c
		| otherwise = '@'
	applyOffset (x, y) = (x + xCenter, y + yCenter)
	getTile mr = case mr of
		Just Room{..} -> case rTile of
			Grass -> '.'
			Sand -> ','
			Snow -> '*'
			Water -> '~'
		Nothing -> 'X'

sortCoordsByStrIdx :: [(Int, Int)] -> [(Int, Int)]
sortCoordsByStrIdx = sortBy coordCompare
	where
	coordCompare (a, b) (a', b')
		| b == b' = compare a a'
		| b < b' = GT
		| otherwise = LT

goDir :: Coord -> Direction -> Coord
goDir (x, y) d = case d of
	East -> (x + 1, y)
	West -> (x - 1, y)
	North -> (x, y + 1)
	South -> (x, y - 1)
