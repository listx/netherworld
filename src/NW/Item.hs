{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NW.Item where

import Control.Monad.Primitive
import System.Random.MWC

import NW.Affix
import NW.Effect
import NW.Random
import NW.Stats

data ItemClass
	= ICGear Gear -- something you wear (armor, weapons)
	| ICPotion -- something you drink to heal/get temporary effects
--	| Scroll -- one-shot magical spells
	deriving (Eq, Show)

instance Variate ItemClass where
	uniform rng = do
		a <- uniformR (0,1 :: Int) rng
		case a of
			0 -> do
				gear <- uniform rng
				return $ ICGear gear
			1 -> return ICPotion
	uniformR _ _ = error "uniformR: ItemClass unsupported"

data Gear
	= GArmor Armor
	| GWeapon Weapon
	| GAccessory Accessory -- pendants, amulets, rings, necklaces, etc.
	deriving (Eq, Show)

data Armor
	= Helm
	| Breastplate
	| Boots
	| Gloves
	| Shield
	deriving (Eq, Enum, Show)

instance Variate Armor where
	uniform rng = return . toEnum =<< uniformR (fromEnum Helm, fromEnum Shield) rng
	uniformR _ _ = error "uniformR: Armor unsupported"

data Weapon
	= Sword
	| Axe
	| Dagger
	| Spear
	| Pike
	| Flail
	| Staff
	deriving (Eq, Enum, Show)

instance Variate Weapon where
	uniform rng = return . toEnum =<< uniformR (fromEnum Sword, fromEnum Staff) rng
	uniformR _ _ = error "uniformR: Weapon unsupported"

data Accessory
	= Ring
	| Necklace -- aka "Pendant", "Amulet"
	| Earring
	| Bracelet
	deriving (Eq, Enum, Show)

instance Variate Accessory where
	uniform rng = return . toEnum =<< uniformR (fromEnum Ring, fromEnum Bracelet) rng
	uniformR _ _ = error "uniformR: Accessory unsupported"

data Item = Item
	{ itemClass :: ItemClass
	, itemAffixes :: [Affix]
	} deriving (Eq, Show)

instance Variate Gear where
	uniform rng = do
		g <- uniformR (0,2 :: Int) rng
		x <- case g of
			0 -> return . GArmor =<< uniform rng
			1 -> return . GWeapon =<< uniform rng
			2 -> return . GAccessory =<< uniform rng
		return x
	uniformR _ _ = error "uniformR: Gear unsupported"
--	uniformR (a, b) rng
--		| a == b = return a
--		| a > b = uniformR (b, a) rng
--		| otherwise = return . toEnum =<< uniformR (fromEnum a, fromEnum b) rng

-- | Generate a random item. This function should be called when a monster dies
-- and we need to create loot.
genRandomItem :: PrimMonad m => AffixDB -> Gen (PrimState m) -> m Item
genRandomItem adb rng = do
	ic <- uniform rng
	affixes <- case ic of
		ICGear _ -> do
			-- You can have up to 2 affixes. If we pick a NAME affix, our 2nd
			-- affix must be an ADJ affix.
			n <- uniformR (0, 2) rng
			rndSample n adb rng
		-- there should be a handful of hardcoded potion types that are
		-- essential to the game, regardless of the kind of game content (e.g.,
		-- health potion, mega health potion, mana potion, elixir, etc>), and
		-- these types should get some priority over other exotic potions
		ICPotion -> return []
	return $ Item
		{ itemClass = ic
		, itemAffixes = affixes
		}

renderItem :: Item -> String
renderItem Item{..} = prefix ++ noun ++ suffix
	where
	noun = case itemClass of
		ICGear g -> case g of
			GArmor a -> show a
			GWeapon w -> show w
			GAccessory c -> show c
		ICPotion -> "Potion"
	(prefix, suffix)
		| null itemAffixes = ("", "")
--		| length itemAffixes > 1 =
		| otherwise = ("", "")
