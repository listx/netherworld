{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Maybe
import System.Exit
import System.FilePath

import NW.Affix
import NW.Config
import NW.Error
import NW.Game
import NW.Map
import NW.Option
import NW.Player
import NW.Random
import NW.State

main :: IO ()
main = do
	opts@Opts{..} <- getOpts
	errNo <- argsCheck opts
	when (errNo > 0) . exitWith $ ExitFailure errNo
	x <- importConfig game_cfg
	config <- case x of
		Left _ -> do
			return Nothing
		Right result -> return $ Just result
	failIfNothing config "Config"
	let
		config' = sanitizeConfig (takeDirectory game_cfg) $ fromJust config
	cfgErrNo <- filesExistCheck $ map ($ config') [cfgMap, cfgAffixDB]
	when (cfgErrNo /= 0) $ exitFailure
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
			, gsDebug = debug_mode
			}
	_ <- gameLoop gs
	return ()
	where
	seedDefault = SeedManual [1..258]
