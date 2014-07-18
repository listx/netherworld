module NW.Error where

import Control.Monad
import Data.List
import Data.Maybe
import System.Exit
import System.IO

import NW.Util

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
fail' got expected = fail ("got: " ++ squote got ++ " --- expected: " ++ optionalMsg ++ quoteExpected)
	where
	quoteExpected = intercalate ", " $ map squote expected
	optionalMsg = if length expected > 1
		then "one of "
		else []

failIfNothing :: Maybe a -> String -> IO ()
failIfNothing a name = when (isNothing a) $ do
	errMsg $ "type " ++ squote name ++ " is Nothing"
	exitFailure
