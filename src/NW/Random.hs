{-# LANGUAGE RecordWildCards #-}

module NW.Random where

import Control.Monad.Primitive
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.List (foldl')
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Data.Word
import System.Random.MWC

data NWSeed
	= SeedManual Word32
	| SeedToday
	| SeedRandom
	deriving (Eq, Show)

mkGen :: NWSeed -> IO GenIO
mkGen s = case s of
	SeedManual n -> initialize' n
	SeedToday -> initialize' . (fromIntegral . toModifiedJulianDay . utctDay)
		=<< getCurrentTime
	SeedRandom -> createSystemRandom

initialize' :: PrimMonad m => Word32 -> m (Gen (PrimState m))
initialize' num = initialize . V.fromList . loop B.empty . B.pack $ octetsLE num
	where
	loop :: B.ByteString -> B.ByteString -> [Word32]
	loop acc bs
		| B.length acc >= (256 * 4) = toW32s acc
		| otherwise = loop
			(B.append acc . B.fromStrict $ SHA1.hashlazy bs)
			(B.fromStrict $ SHA1.hashlazy bs)
	toW32s :: B.ByteString -> [Word32]
	toW32s = map fromOctetsLE . chop 4 . B.unpack

rndSelect :: Int -> [a] -> Gen (PrimState IO) -> IO [a]
rndSelect 0 _ _ = return []
rndSelect _ [] _ = return []
rndSelect count xs gen = do
	idx <- uniformR (0, (length xs) - 1) gen
	rest <- rndSelect (count - 1) (snd $ removeAt (idx + 1) xs) gen
	return ((xs !! idx) : rest)

removeAt :: Int -> [a] -> (a, [a])
removeAt n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)

octetsLE :: Word32 -> [Word8]
octetsLE w = map (fromIntegral . uncurry shiftR)
	$ zip (repeat w) [0, 8, 16, 24]

octetsBE :: Word32 -> [Word8]
octetsBE = reverse . octetsLE

fromOctetsBE :: [Word8] -> Word32
fromOctetsBE = foldl' accum 0
	where
	accum a o = (a `shiftL` 8) .|. fromIntegral o

fromOctetsLE :: [Word8] -> Word32
fromOctetsLE = fromOctetsBE . reverse

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

randApply :: PrimMonad m => a -> (a -> a) -> (Int, Int) -> Gen (PrimState m) -> m a
randApply a f (x, y) rng
	| x < 1 = error "randApply: numerator x is less than 1"
	| y < 2 = error "randApply: denominator d is less than 2"
	| x >= y = error "randApply: numerator is greater than denominator"
	| otherwise = do
		roll <- uniformR (0, y - 1) rng
		if (roll < x)
			then return (f a)
			else return a
