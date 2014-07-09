{-# LANGUAGE RecordWildCards #-}

module NW.Util where

import Data.Char

downcase :: String -> String
downcase = map toLower

squote :: String -> String
squote x = "`" ++ x ++ "'"

enumsHash :: Show a => [a] -> [(String, a)]
enumsHash enums = zip (map (downcase . show) enums) enums
