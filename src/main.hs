{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe
import System.FilePath
import System.Environment

import NW.Affix
import NW.Config
import NW.Error
import NW.Game
import NW.Map
import NW.Player
import NW.Random
import NW.State

main :: IO ()
main = do
	args <- getArgs
	let
		cfg = head args
	x <- importConfig cfg
	config <- case x of
		Left _ -> do
			return Nothing
		Right result -> return $ Just result
	failIfNothing config "Config"
	let
		config' = sanitizeConfig (takeDirectory cfg) $ fromJust config
	gameMap <- importMap $ cfgMap config'
	affixDB <- importGMAffixes $ cfgAffixDB config'
	failIfEmpty affixDB "AffixDB"
	rng <- mkGen seedDefault
	let
		gs = GameState
			{ gsConfig = config'
			, gsGameMap = gameMap
			, gsPlayer = initPlayer gameMap
			, gsMonsters = []
			, gsLastCommand = ""
			, gsLastBattleCommand = ""
			, gsAffixDB = affixDB
			, gsMonsterDB = []
			, gsRng = rng
			, gsRngInitial = [1..258]
			, gsInputHistory = []
			, gsReplay = False
			, gsDebug = True
			}
	_ <- gameLoop gs
	return ()
	where
	seedDefault = SeedManual [1..258]
