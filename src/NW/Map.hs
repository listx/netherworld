{-# LANGUAGE RecordWildCards #-}

module NW.Map where

type Coord = (Int, Int)

data Map = Map
	{ rawStrs :: [String]
	} deriving (Eq, Show)

importMap :: FilePath -> IO Map
importMap fp = do
	f <- readFile fp
	return $ Map
		{ rawStrs = lines f
		}

mapSize :: Map -> Coord
mapSize Map{..} = (x, y)
	where
	x = length $ rawStrs!!0
	y = length rawStrs

padAmt :: Int
padAmt = 10
