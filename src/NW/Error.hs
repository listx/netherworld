module NW.Error where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.Exit
import System.IO

import NW.Util

dbgMsg :: Bool -> String -> IO ()
dbgMsg b str
	| b = putStrLn $ "debug: " ++ str
	| otherwise = return ()

errMsg :: String -> IO ()
errMsg = hPutStr stderr . ("error: " ++)

errMsgn :: String -> IO ()
errMsgn = hPutStrLn stderr . ("error: " ++)

fatal :: String -> Int -> IO ()
fatal msg errNo = do
	errMsg msg
	exitWith $ ExitFailure errNo

-- | A more user-friendly way to generate a failure message upon a failed parse.
fail' :: Monad m => String -> [String] -> m a
fail' got expected = fail
	$ "got: " ++ squote got ++ " --- expected: " ++ optionalMsg ++ quoteExpected
	where
	quoteExpected = intercalate ", " $ map squote expected
	optionalMsg = if length expected > 1
		then "one of "
		else []

failIfNothing :: Maybe a -> String -> IO ()
failIfNothing a name = when (isNothing a) $ do
	errMsg $ "type " ++ squote name ++ " is Nothing"
	exitFailure

failIfEmpty :: [a] -> String -> IO ()
failIfEmpty a name = when (null a) $ do
	errMsg $ "type " ++ squote name ++ " is empty"
	exitFailure

fileExistCheck :: FilePath -> IO Int
fileExistCheck fname = do
	fileExists <- doesFileExist fname
	if fileExists
		then return 0
		else do
			errMsgn $ "file" ++ squote' fname ++ "does not exist"
			return 1

filesExistCheck :: [FilePath] -> IO Int
filesExistCheck fs = do
	errNos <- mapM fileExistCheck fs
	if (all (==0) errNos)
		then return 0
		else return 1
