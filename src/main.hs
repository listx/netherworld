{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Data.Maybe
import System.FilePath
import System.Environment
import System.Random.MWC
import qualified Data.Vector.Unboxed as VU

import NW.Affix
import NW.Battle
import NW.Config
import NW.Error
import NW.Map
import NW.Player
import NW.Random
import NW.State
import NW.Stats
import NW.Util

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
			, gsPlayer = player gameMap
			, gsMonsters = []
			, gsLastCommand = ""
			, gsLastBattleCommand = ""
			, gsAffixDB = affixDB
			, gsMonsterDB = []
			, gsRng = rng
			, gsRngInitialState = [1..258]
			, gsInputHistory = []
			, gsReplay = False
			}
	gameLoop gs
	where
	player gm = Player
		{ playerCoord = firstCoord gm
		, playerStats = statsBaseDefault
		}
	seedDefault = SeedManual [1..258]

gameLoop :: GameState -> IO ()
gameLoop gs = do
	putStrLn $ miniMapView m playerCoord
	putStrLn $ show playerCoord
	if (gsReplay gs) && null (gsInputHistory gs)
		then do
			putStrLn "End of game move history."
			return ()
		else do
			(gs1, str) <- getUserInput gs
			let
				tokens = words str
			if length tokens > 0
				then if
					| elem (head tokens) directionals -> goIfOk gs1 str
					| otherwise -> case head tokens of
						"quit" -> return ()
						"q" -> return ()
						"save"
							| length tokens /= 2 -> do
								putStrLn "Please provide a single savegame filepath."
								gameLoop gs1
							| otherwise -> do
								saveGame gs1 (tokens!!1)
								gameLoop gs1
						_ -> do
							putStrLn "You stall in confusion."
							gameLoop gs1
				else goIfOk gs1 (gsLastCommand gs1)
	where
	directionals = ["e", "w", "n", "s", "ne", "nw", "se", "sw"]
	m@GameMap{..} = gsGameMap gs
	Player{..} = gsPlayer gs

goIfOk :: GameState -> String -> IO ()
goIfOk gs@GameState{..} str
	| inRange m c = case midx gameMapVector c of
		Just _ -> do
			let
				gs1 = gs
					{ gsGameMap = m
					, gsPlayer = p {playerCoord = c}
					, gsLastCommand = str
					}
			r <- roll 100 gsRng
			if (r < 7)
				then do
					putStrLn "You enter a battle!!!"
					gs2 <- spawnMonsters gs1
					gs3 <- battleLoop gs2
					gameLoop gs3
				else gameLoop gs1
		Nothing -> do
			putStrLn "You cannot go there."
			gameLoop gs
				{ gsLastCommand = str
				}
	| otherwise = do
		putStrLn "You cannot go there."
		gameLoop gs
			{ gsLastCommand = str
			}
	where
	d = case str of
		"e" -> [East]
		"w" -> [West]
		"n" -> [North]
		"s" -> [South]
		"ne" -> [North, East]
		"nw" -> [North, West]
		"se" -> [South, East]
		_ -> [South, West]
	c = foldl goDir playerCoord d
	m@GameMap{..} = gsGameMap
	p@Player{..} = gsPlayer

-- | Save the game. If we're doing a replay, don't do anything.
saveGame :: GameState -> FilePath -> IO ()
saveGame GameState{..} fp
	| gsReplay = return ()
	| otherwise = do
		let
			rngStateInitial = unlines . map unwords . chop 8 . map (indent . show) $ gsRngInitialState
		rngState <- (unlines . map unwords . chop 8 . map (indent . show) . VU.toList . fromSeed) <$> save gsRng
		writeFile fp $ unlines
			[ saveShow gsConfig
			, saveShow gsPlayer
			, "last-command " ++ show gsLastCommand ++ "\n"
			, "rng-state-initial\n" ++ rngStateInitial
			, "rng-state\n" ++ rngState
			, "input-history " ++ show gsInputHistory
			]

class Save a where
	saveShow :: a -> String

instance Save Config where
	saveShow Config{..} = unlines
		[ "map " ++ show cfgMap
		, "affix-db " ++ show cfgAffixDB
		, "monster-db " ++ show cfgMonsterDB
		]

instance Save Player where
	saveShow Player{..} = unlines
		[ "player-coord " ++ show x ++ " " ++ show y
		, "player-stats " ++ "{\n" ++ unlines (map (indent . showTuple) playerStats) ++ "}"
		]
		where
		x = fst playerCoord
		y = snd playerCoord
