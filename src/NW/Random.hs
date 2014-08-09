{-# LANGUAGE RecordWildCards #-}

module NW.Random where

import Control.Monad.Primitive
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.List (foldl', nub, transpose)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Data.Word
import System.Random.MWC

import NW.Util

data NWSeed
	= SeedEmpty
	| SeedManual [Word32]
	| SeedToday
	| SeedRandom
	deriving (Eq, Show)

-- | For SeedManual, we limit [Word32] to 258 elements, because that is the
-- maximum number of elements System.Random.MWC deals with.
mkGen :: NWSeed -> IO GenIO
mkGen s = case s of
	SeedEmpty -> initialize $ V.empty
	SeedManual ns -> initialize $ initialize' ns
	SeedToday -> initialize
		. initialize'
		. ((:[]) . fromIntegral . toModifiedJulianDay . utctDay)
		=<< getCurrentTime
	SeedRandom -> createSystemRandom

-- | The MWC RNG internally uses a state of 256 Word32's (`ws`), an index value
-- `i` that wraps around 0-255 (a single 'byte' in C), and a mutable constant
-- value `c` that changes with each iteration of the RNG. For simplicity, both
-- `i` and `c` are Word32 values.
initialize' :: [Word32] -> V.Vector Word32
initialize' ns
	| any (==0) wsc = error "one or more values in either `ws` or `c` is 0"
	| length (nub ws) /= length ws = error "some values in `ws` are the same"
	| otherwise = set_i_c
		. V.fromList . xorJoin
		$ map (shaInflate B.empty . B.pack . octetsLE) ws
	where
	xorJoin :: [[Word32]] -> [Word32]
	xorJoin = map (foldl' xor 0) . transpose
	ws = take 256 ns
	wsc = ws ++ drop 257 ns
	ic = drop 256 ns
	set_i_c v
		| length ns == 258 = v V.++ (V.fromList ic)
		| otherwise = v

shaInflate :: B.ByteString -> B.ByteString -> [Word32]
shaInflate acc bs
	| B.length acc >= (256 * 4) = take 256 $ toW32s acc
	| otherwise = shaInflate
		(B.append acc . B.fromStrict $ SHA1.hashlazy bs)
		(B.fromStrict $ SHA1.hashlazy bs)

toW32s :: B.ByteString -> [Word32]
toW32s = map fromOctetsLE . chop 4 . B.unpack

warmup :: Int -> GenIO -> IO Word64
warmup n rng
	| n < 1 = error "n must be at least 1"
	| otherwise = do
		runs <- mapM (\_ -> uniform rng :: IO Word64) [1..n]
		return $ last runs

-- | Randomly sample n elements from a list.
rndSample :: PrimMonad m => Int -> [a] -> Gen (PrimState m) -> m [a]
rndSample 0 _ _ = return []
rndSample _ [] _ = return []
rndSample count xs gen
	| count < 0 = error "count must be at least 0"
	| otherwise = do
		idx <- uniformR (0, (length xs) - 1) gen
		rest <- rndSample (count - 1) (snd $ removeAt idx xs) gen
		return ((xs !! idx) : rest)

-- | Randomly select a single element from a list.
rndSelect :: PrimMonad m => [a] -> Gen (PrimState m) -> m a
rndSelect xs rng
	| null xs = error "choose: list is empty"
	| otherwise = do
		idx <- uniformR (0, length xs - 1) rng
		return $ xs !! idx

removeAt :: Int -> [a] -> (a, [a])
removeAt n = (\(a, b) -> (head b, a ++ tail b)) . splitAt n

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

randApply :: PrimMonad m => a -> (a -> a) -> (Int, Int) -> Gen (PrimState m) -> m a
randApply a f (x, y) rng
	| x < 1 = error "randApply: numerator x is less than 1"
	| y < 2 = error "randApply: denominator d is less than 2"
	| x >= y = error "randApply: numerator is greater than denominator"
	| otherwise = do
		r <- uniformR (0, y - 1) rng
		if (r < x)
			then return (f a)
			else return a

roll :: PrimMonad m => Int -> Gen (PrimState m) -> m Int
roll n
	| n < 2 = error "roll argument less than 2"
	| otherwise = uniformR (1, n)
