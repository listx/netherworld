module NW.Error where

import Control.Monad
import Data.List
import Data.Maybe
import System.Exit

import NW.Util

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
	putStrLn $ squote name ++ " error"
	exitFailure
