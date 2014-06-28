{-# LANGUAGE RecordWildCards #-}

module NW.Map where

import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

-- | A tile is a 1-square area that the player can occupy in the game map.
data Tile
	= Grass
	| Sand
	| Snow
	| Water
	deriving (Eq, Show)

type Coord = (Int, Int)

type GameMapHash = M.Map Coord (Maybe Tile)

data GameMap = GameMap
	{ gameMap :: GameMapHash
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
		revSrc = T.unlines . reverse $ T.lines src
		tileHash = fst $ T.foldl mkTile (M.empty, (0, 0)) revSrc
	return $ GameMap
		{ gameMap = tileHash
		}
	where
	mkTile (m, (x, y)) c = case c of
		'\n' -> (m, (0, y + 1))
		c' -> ((M.insert coord (charToTile c') m), coordNew)
		where
		coord = (x, y)
		coordNew = (x + 1, y)

charToTile :: Char -> Maybe Tile
charToTile c = case c of
	'.' -> Just Grass
	',' -> Just Sand
	'*' -> Just Snow
	'~' -> Just Water
	_ -> Nothing

padAmt :: Int
padAmt = 10
