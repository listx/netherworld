--{-# LANGUAGE MultiWayIf #-}
--{-# LANGUAGE RecordWildCards #-}

module NW.Game where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector.Unboxed as VU
import Data.Word
import System.Exit
import System.IO
import System.Random.MWC
import Text.Parsec.Combinator
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Text.Lazy
import Text.Printf

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

dirs :: [String]
dirs = map fst dirHash

dirHash :: [(String, [Direction])]
dirHash =
	[ ("e", [East])
	, ("w", [West])
	, ("n", [North])
	, ("s", [South])
	, ("ne", [North, East])
	, ("nw", [North, West])
	, ("se", [South, East])
	, ("sw", [South, West])
	]

gameLoop :: GameState -> IO GameState
gameLoop gs = do
	nwPuts gs $ miniMapView m pc
	nwPuts gs $ show pc
	when (gsReplay gs) $ do
		dbgMsg (gsDebug gs) $ "replay: playerCoord: " ++ show pc
	if (gsReplay gs) && null (gsInputHistory gs)
		then do
			dbgMsg (gsDebug gs) "End of game move history."
			dbgMsg (gsDebug gs) $ miniMapView m pc
			dbgMsg (gsDebug gs) $ show pc
			return gs
		else do
			(gs1, str) <- getUserInput gs
			let
				tokens = words str
				lastCommand = gsLastCommand gs1
				cmd
					| length tokens > 0 = head tokens
					| not (null lastCommand) = lastCommand
					| otherwise = ""
			if
				| elem cmd dirs -> do
					gs2 <- goIfOk gs1 {gsLastCommand = cmd} cmd
					let
						pc' = playerCoord $ gsPlayer gs2
					if (pc /= pc')
						then do
							mixRng gs2 cmd
							gs3 <- battleTrigger gs2
							gameLoop gs3
						else gameLoop gs2
				| otherwise -> case cmd of
					"quit" -> return gs1
					"q" -> return gs1
					"save"
						| length tokens /= 2 -> do
							nwPuts gs1 "Please provide a single savegame filepath."
							gameLoop gs1
						| otherwise -> do
							saveGame gs1 (tokens!!1)
							gameLoop gs1
					"load"
						| length tokens /= 2 -> do
							nwPuts gs1 "Please provide a single savegame filepath."
							gameLoop gs1
						| otherwise -> do
							gsx <- importGame (gsDebug gs) (tokens!!1)
							putStrLn "Game loaded successfully."
							putStrLn "Entering world..."
							gameLoop gsx
					"" -> do
						nwPuts gs1 "Confused already?"
						gameLoop gs1
					_ -> do
						nwPuts gs1 "You stall in confusion."
						gameLoop gs1
	where
	m = gsGameMap gs
	pc = playerCoord $ gsPlayer gs

goIfOk :: GameState -> String -> IO GameState
goIfOk gs@GameState{..} str
	| inRange m c = case midx gameMapVector c of
		Just _ -> return gs
			{ gsPlayer = p {playerCoord = c}
			}
		Nothing -> do
			nwPuts gs "You cannot go there."
			return gs
	| otherwise = e
	where
	c = foldl goDir playerCoord $ case lookup str dirHash of
		Just dir -> dir
		Nothing -> e
	m@GameMap{..} = gsGameMap
	p@Player{..} = gsPlayer
	e = error $ "unknown direction" ++ squote' str

-- Mix RNG based on directional input.
mixRng :: GameState -> String -> IO ()
mixRng GameState{..} str = do
	warmups <- rndSample 8 [1..8] gsRng
	let
		dirRngWarmups = zip dirs warmups
	case lookup str dirRngWarmups of
		Just n -> void $ warmup n gsRng
		Nothing -> return ()

-- | Parse savegame file and load it into the game, replaying everything until
-- we run out of game history. Before we return the clean game state, we have to
-- reassign the gsInputHistory to what it was before, because this history must
-- always store the *entire* game history, for game validation purposes. We also
-- set the replay flag to false, to allow user interaction.
importGame :: Bool -> FilePath -> IO GameState
importGame debug fname = do
	fsrc <- T.readFile fname
	(parseSuccess, (gs, rng)) <- parseSaveGame fname fsrc
	when (not parseSuccess) $ do
		errMsgn "importGame: parse failure"
		exitFailure
	dbgMsg (gsDebug gs) $ "importGame: " ++ show (gsInputHistory gs)
	game <- verifyGame gs {gsReplay = True, gsDebug = debug} rng
	if isJust game
		then do
			let
				game' = fromJust game
			return $ game'
				{ gsReplay = False
				, gsInputHistory = reverse (gsInputHistory gs)
				}
		else do
			errMsgn "game verification failure"
			exitFailure

-- | Simulate a game by running all input history. Then, see if certain
-- important game data match up.
verifyGame :: GameState -> [Word32] -> IO (Maybe GameState)
verifyGame gs rngState = do
	gameMap <- importMap $ cfgMap (gsConfig gs)
	affixDB <- importGMAffixes $ cfgAffixDB (gsConfig gs)
	failIfEmpty affixDB "AffixDB"
	rngInitial <- mkGen . SeedManual $ gsRngInitial gs
	let
		gs0 = gs
			{ gsGameMap = gameMap
			, gsPlayer = initPlayer gameMap
			, gsRng = rngInitial
			}
	gs1 <- gameLoop gs0
	dbgMsg (gsDebug gs1) "verifyGame's gameLoop finished."
	dbgMsg (gsDebug gs1) $ "gsLastCommand: " ++ show (gsLastCommand gs)
	dbgMsg (gsDebug gs1) $ "gsInputHistory: " ++ show (gsInputHistory gs)
	s <- (VU.toList . fromSeed) <$> save (gsRng gs1)
	if
		| rngState /= s -> e "rng mismatch"
		| gsPlayer gs /= gsPlayer gs1 -> e "player mismatch"
		| otherwise -> return $ Just gs1
	where
	e m = do
		errMsgn $ "verifyGame: " ++ m
		return Nothing

-- | Save the game. If we're doing a replay, don't do anything.
saveGame :: GameState -> FilePath -> IO ()
saveGame GameState{..} fp
	| gsReplay = return ()
	| otherwise = do
		putStrLn $ show gsInputHistory
		let
			rngInitial = unlines . map unwords . chop 8 . map (indent . printf "0x%08x") $ gsRngInitial
		rng <- (unlines . map unwords . chop 8 . map (indent . printf "0x%08x") . VU.toList . fromSeed) <$> save gsRng
		writeFile fp $ unlines
			[ saveShow gsConfig
			, saveShow gsPlayer
			, "last-command " ++ show gsLastCommand ++ "\n"
			, "rng-initial\n" ++ rngInitial
			, "rng\n" ++ rng
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

parseSaveGame :: String -> T.Text -> IO (Bool, (GameState, [Word32]))
parseSaveGame fname src = case Prim.parse saveGameParser fname src of
	Left parseError -> do
		errMsg "saveGameParser: could not parse file "
		hPutStrLn stderr (show parseError)
		return (False, (gsDefault, []))
	Right result -> return (True, result)

saveGameParser :: Parser (GameState, [Word32])
saveGameParser = do
	_ <- t_whiteSpace
	config <- configParser
	player <- playerParser
	lc <- lastCommandParser
	_ <- t_symbol "rng-initial"
	rngInitial <- map fromIntegral <$> rngParser
	_ <- t_symbol "rng"
	rng <- map fromIntegral <$> rngParser
	_ <- t_symbol "input-history"
	hist <- reverse <$> t_brackets (sepBy1 t_stringLiteral (t_symbol ","))
	eof
	return . flip (,) rng $ gsDefault
		{ gsConfig = config
		, gsPlayer = player
		, gsLastCommand = lc
		, gsRngInitial = rngInitial
		, gsInputHistory = hist
		}

playerParser :: Parser Player
playerParser = do
	_ <- t_whiteSpace
	_ <- t_symbol "player-coord"
	x <- intParser'
	y <- intParser'
	_ <- t_symbol "player-stats"
	stats <- t_braces (many1 statParser)
	return Player
		{ playerCoord = (x, y)
		, playerStats = stats
		}

statParser :: Parser (Attribute, Int)
statParser = do
	attr <- choice' $ map (\a -> t_symbol (show a) >> return a)
		[ Health
		, Mana
		, Strength
		, Wisdom
		]
	n <- intParser'
	return (attr, n)

lastCommandParser :: Parser String
lastCommandParser = do
	_ <- t_symbol "last-command"
	t_stringLiteral

rngParser :: Parser [Int]
rngParser = count 258 hexParser'
