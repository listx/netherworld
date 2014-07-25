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
			_ -> return ICPotion
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
			_ -> return . GAccessory =<< uniform rng
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
			-- affix must be an ADJ affix. NAME affixes are generally more
			-- powerful (at least, they are supposed to be more powerful), so
			-- there is only a small percent chance that we'll get a NAME affix.

			-- The possibilities are:
			-- * 1 adj only ("steel gloves") (can't just be 'gloves' because that's too barren)
			-- * 1 proper noun suffix ("gloves of the bear")
			-- * 1 noun suffix ("shield of doom")
			-- * 1 persona ("assassin's dagger" or "dagger of the assassin")
			-- * 1 name only ("daniel's gloves" or "shield of achilles")

			-- For simplicity's sake, we only choose 1 affix.
			ac <- uniform rng
			affix <- rndSelect (filter ((== ac) . affixClass) adb) rng
			mapM (rasterizeAffix rng) [affix]
		-- there should be a handful of hardcoded potion types that are
		-- essential to the game, regardless of the kind of game content (e.g.,
		-- health potion, mega health potion, mana potion, elixir, etc>), and
		-- these types should get some priority over other exotic potions
		ICPotion -> return []
	return $ Item
		{ itemClass = ic
		, itemAffixes = affixes
		}


-- | The only modifier that affects the player at a variable rate is damage done; all others (health, mana, defense, resistance, life steal, etc.) affect the character at a constant rate. Thus, we "rasterize" any NVRange value we find for damage modifiers.
rasterizeAffix :: PrimMonad m => Gen (PrimState m) -> Affix -> m Affix
rasterizeAffix rng affix@Affix{..} = do
	es <- mapM (rasterizeEffect rng) affixEffects
	return $ affix
		{ affixEffects = es
		}

-- | Only rasterize non-damage-dealing effects.
rasterizeEffect :: PrimMonad m => Gen (PrimState m) -> Effect -> m Effect
rasterizeEffect rng effect@(et, nv) = case et of
	EAttribute Damage -> return effect
	EAttribute (DamageE _) -> return effect
	_ -> case nv of
		NVRange (a, b) -> do
			c <- uniformR (a, b) rng
			return (et, (NVConst c))
		_ -> return effect

-- | The higher the rating, the more "valuable" the item, all things equal. This
-- function is useful for at least two situations: (1) random item generation
-- and (2) item shops (merchant AI's valuation of an item).
itemRating :: Item -> Int
itemRating Item{..} = sum $ map affixRating itemAffixes

affixRating :: Affix -> Int
affixRating Affix{..} = affixClassRating + (sum $ map effectRating affixEffects)
	where
	affixClassRating = case affixClass of
		ACAdj -> 10
		ACNoun -> 20
		ACNounProper -> 20
		ACPersona -> 30
		ACName -> 40

effectRating :: Effect -> Int
effectRating (effectType, nv) = effectTypeRating effectType * nvRating nv

effectTypeRating :: EffectType -> Int
effectTypeRating et = case et of
	EAttribute Health -> 10
	EAttribute Mana -> 10
	EAttribute Strength -> 10
	EAttribute Wisdom -> 10
	EAttribute Attack -> 7
	EAttribute MAttack -> 7
	EAttribute Defense -> 7
	EAttribute MDefense -> 7
	EAttribute Damage -> 9
	EAttribute (DamageE _) -> 5
	EAttribute (Resist _) -> 5
	EAttribute LifeSteal -> 8
	EGameMechanics MagicItemFind -> 3
	EGameMechanics GoldEarned -> 2

nvRating :: NumberVal -> Int
nvRating (NVConst n) = n
nvRating (NVPerc n) = n
nvRating (NVRange (a, b)) = div (sum [a..b]) $ length [a..b]

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
		| otherwise = case affixClass of
			ACAdj -> (affixName ++ " ", "")
			ACNoun -> ("", " of " ++ affixName)
			ACNounProper -> ("", " of the " ++ affixName)
			ACPersona -> (affixName ++ "'s ", "") -- "Killer's" or "Assassin's"
			ACName -> ("", " of " ++ affixName)
		where
		Affix{..} = head itemAffixes
