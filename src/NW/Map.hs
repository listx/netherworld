{-# LANGUAGE RecordWildCards #-}

module NW.Map where

--import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

-- | A tile is a 1-square area that the player can occupy in the game map.
data Tile
	= Grass
	| Sand
	| Snow
	| Water
	deriving (Eq, Show)

type Coord = (Int, Int)

data Room = Room
	{ rTile :: Tile
--	, rKey :: Maybe RoomKey -- if Just RoomKey, requires that particular key to be possessed by the player to enter this room
--	, rMobProb :: Int -- probably index of commencing a random fight encounter when entering this tile; 0 for no-fight zones, and 100 for always-fight zones
--	, rPModifier :: (PlayerStats -> PlayerStats) -- a modifier function that affects the player's stats (e.g., +10 fright for scary zones)
	} deriving (Eq, Show)

roomDefault :: Room
roomDefault = Room
	{ rTile = Grass
	}

type GameMapVector = V.Vector (V.Vector (Coord, Maybe Room))

data GameMap = GameMap
	{ gameMapVector :: GameMapVector
	, gameMapRange :: (Int, Int)
	} deriving (Eq, Show)

-- | When we import a map, we first reverse the lines, and then access the line
-- one character at a time. The coordinate system we use sets (0, 0) as the very
-- first tile we process (i.e., (0,0) is the first character of the last line in
-- the imported map). Because of this system, we have to reverse the lines
-- before processing them. 'mkTile' simply converts each character encountered,
-- and increments the x and y indices accordingly (x on each character, y on
-- each newline encountered).

importMap :: FilePath -> IO GameMap
importMap fp = do
	src <- T.readFile fp
	let
		srcLines = T.lines src
		revSrc = T.unlines $ reverse srcLines
		mapSizeX = fromIntegral . maximum $ map T.length srcLines
		mapSizeY = length srcLines
		hash :: V.Vector [(Coord, Maybe Room)]
		hash = (\(_, _, rows) -> V.fromList $ reverse rows) $ T.foldl mkRoom ((0, 0), [], []) revSrc -- hash is a Vector, w/ each element (row) containing a LIST of coords
	-- TODO: check input bounds
	return $ GameMap
		{ gameMapVector = V.map rowWise (V.zip (gmv (mapSizeX, mapSizeY)) $ hash)
		, gameMapRange = (mapSizeX, mapSizeY)
		}
	where
	gmv :: (Int, Int) -> GameMapVector
	gmv (sizeX, sizeY) = V.generate sizeY (\y -> V.generate sizeX (\x -> ((x, y), Nothing)))
	mkRoom (coord@(x, y), row, rows) c = case c of
			' ' -> skip
			-- skip to next Y level; since newline determines whether the
			-- current row gets inserted, this means that all maps MUST end with
			-- a newline... thankfully, 'unlines' always adds newlines
			'\n' -> ((0, y + 1), [], row:rows)
			_ -> (coordNew, (coord, Just . setRoom $ charToTile c):row, rows)
		where
		skip = (coordNew, row, rows)
		coordNew = (x + 1, y)
		setRoom t = Room
			{ rTile = t
			}
	rowWise :: (V.Vector (Coord, Maybe Room), [(Coord, Maybe Room)]) -> V.Vector (Coord, Maybe Room)
	rowWise (nothingsRow, xcoordRooms) = nothingsRow V.// xcoordRooms'
		where
		xcoordRooms' = map (\(coord@(x, _), mr) -> (x, (coord, mr))) xcoordRooms

charToTile :: Char -> Tile
charToTile c = case c of
	',' -> Sand
	'*' -> Snow
	'~' -> Water
	_ -> Grass -- '.' is the preferred char for representing grass

padAmt :: Int
padAmt = 10

midx :: GameMapVector -> Coord -> Maybe Room
midx gmv (x, y) = snd $ gmv V.! y V.! x

inRange :: GameMap -> Coord -> Bool
inRange GameMap{..} (x, y) = x >= 0 && x < xMax && y >= 0 && y < yMax
	where
	(xMax, yMax) = gameMapRange
