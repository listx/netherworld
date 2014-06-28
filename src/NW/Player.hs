{-# LANGUAGE RecordWildCards #-}

module NW.Player where

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
	buildLine y' = reverse $ foldl searchRange2 [] [(x-padAmt)..(x+padAmt)]
		where
		searchRange2 acc idx
			| idx >= 0 && idx < xMax && y' >= 0 && y' < yMax = (rawStrs!!y')!!idx:acc
			| otherwise = borderChar:acc
	searchRange acc idx
		| idx >= 0 && idx < xMax = playerLine'!!idx:acc
		| otherwise = borderChar:acc
	borderChar = toEnum 0x25a0

playerLine :: Player -> Map -> String
playerLine Player{..} Map{..} = rawStrs!!(snd playerCoord)

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
