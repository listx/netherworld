{-# LANGUAGE RecordWildCards #-}

module NW.Player where

import Data.List
import qualified Data.Map.Lazy as M

import NW.Map

data Player = Player
	{ playerCoord :: Coord
	}

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
miniMapView :: GameMap -> Int -> Coord -> String
miniMapView GameMap{..} n coordCenter@(xCenter, yCenter)
	= intercalate "\n"
	$ map (map (coordToChar . applyOffset) . genToEasternEdge) westernEdge
	where
	miniMapLen = n * 2 + 1
	westernEdge :: [(Int, Int)]
	westernEdge = zip (replicate miniMapLen (-n)) [n,(n-1)..]
	genToEasternEdge (x, y) = [(x', y) | x' <- take miniMapLen [x..]]
	coordToChar :: (Int, Int) -> Char
	coordToChar c
		| c /= coordCenter = case M.lookup c gameMap of
			Just maybeTile -> case maybeTile of
				Just t -> case t of
					Grass -> '.'
					Sand -> ','
					Snow -> '*'
					Water -> '~'
				Nothing -> toEnum 0x25a0 -- map has this coordinate, but the tile type is unrecognized
			Nothing -> 'X' -- map has no such coordinate
		| otherwise = '@'
	applyOffset (x, y) = (x + xCenter, y + yCenter)

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
