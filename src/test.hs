{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import Data.Word
import System.Random.MWC

import NW.Random

main :: IO ()
main = do
	mapM_ warmupShow
		[ SeedEmpty
		, SeedManual [1..255]

		-- Different than [1..255], because we have 1 more value that gets read
		-- into the state.
		, SeedManual [1..256]

		-- Same as [1..256], because length is not 258, and first 256 values are
		-- the same.
		, SeedManual [1..257]

		-- Different than [1..256], because there are exactly 258 values.
		, SeedManual [1..258]

		-- Same as [1..256], because length is not 258, and first 256 values are
		-- the same.
		, SeedManual [1..259]
		, SeedToday
		]
	where
	warmupShow seed = do
		rng <- mkGen seed
		n <- warmup 100000 rng
		putStrLn $ show n
