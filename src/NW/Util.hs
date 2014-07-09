{-# LANGUAGE RecordWildCards #-}

module NW.Util where

import Data.Char
import Data.List

downcase :: String -> String
downcase = map toLower

squote :: String -> String
squote x = "`" ++ x ++ "'"

-- | A more user-friendly way to generate a failure message upon a failed parse.
fail' :: Monad m => String -> [String] -> m a
fail' got expected = fail ("got: " ++ squote got ++ " --- expected: " ++ optionalMsg ++ quoteExpected)
	where
	quoteExpected = intercalate ", " $ map squote expected
	optionalMsg = if length expected > 1
		then "one of "
		else []

enumsHash :: Show a => [a] -> [(String, a)]
enumsHash enums = zip (map (downcase . show) enums) enums
