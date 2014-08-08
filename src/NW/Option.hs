{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module NW.Option where

import System.Console.CmdArgs.Implicit

import NW.Error
import NW.Meta

data Opts = Opts
	{ debug_mode :: Bool
	, game_cfg :: FilePath
	} deriving (Data, Eq, Show, Typeable)

progOpts :: Opts
progOpts = Opts
	{ debug_mode = False &= help "enable Debug mode"
	, game_cfg = "" &= help "game configuration file"
	}

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= versionArg [explicit, name "version", name "v", summary _prog_info]
	&= summary (_prog_info ++ ", " ++ _copyright)
	&= help _prog_summary
	&= helpArg [explicit, name "help", name "h"]
	&= program _prog_name

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| null game_cfg = errMsgn "--game-cfg is undefined" >> return 1
	| otherwise = do
		errNo <- filesExistCheck [game_cfg]
		return $ if (errNo /= 0)
			then 1
			else 0
