{-# LANGUAGE RecordWildCards #-}

module NW.Monster where

import NW.Stats

data Monster = Monster
	{ mStatsBase :: [Stat]
	}
