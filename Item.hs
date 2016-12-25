module Item where

data Weapon = Weapon ClassWeapon Name Damage AttackSpeed deriving (Show, Eq)

data SetItem = SetItem ClassItemSet Defense Name LevelItem deriving (Show, Eq)

data Money = Gold Int Money | Silver Int deriving (Show, Eq)

data ClassItemSet = Armor | Pant | Gloves | Shield | Boots | Helm | Pendant | Ring deriving (Show, Eq)
              
data ClassWeapon = Dagger | Blade | GreatSword | Hammer | Maze | Fists | Wand | Stick | Bow | CrossBow | Guns | Axe deriving (Show, Eq)

data Item = W Weapon | S SetItem | M Money deriving (Show, Eq)

type Name = String

type Damage = (Double,Double)

type AttackSpeed = Int

type LevelItem = Int

type Defense = Int


----------------------------
money :: Item
money = M $ Gold 2 $ Silver 10

aItemSet :: Item
aItemSet = S $ SetItem Ring 2 "ring of noob" 1

newWeapon :: Item
newWeapon = W $ Weapon Blade "Final Blade" (43, 190) 35
