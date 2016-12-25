module Drop (generateNumber, generateItemByLevel, generateRandomItem) where

import Item
import System.Random
import System.IO
import System.IO.Unsafe

oneTo10 :: (Integer,Integer)
oneTo10 = (1,10)

oneTo100 :: (Integer,Integer)
oneTo100 = (1,100)

generateRandomItem :: Item
generateRandomItem = let level = generateNumber oneTo10
                     in let percentage = generateNumber oneTo100
                        in generateItemByLevel level percentage 

generateNumber :: (Integer,Integer) -> Integer
generateNumber range = unsafePerformIO $ getStdRandom $ randomR $ range

generateItemByLevel :: Integer -> Integer -> Item
generateItemByLevel level percentage | percentage <= 10 = W $ generateRandomWeapon level
                                     | percentage > 10 && percentage <= 30 = S $ generateRandomSetItem level
                                     | otherwise = M $ generateRandomMoney level


generateRandomMoney :: Integer -> Money
generateRandomMoney level | level <= 3 = Silver $ (fromIntegral $ generateNumber (100,200))
                          | level > 3 && level <= 6 = Silver $ (fromIntegral $ generateNumber (800,900))
                          | otherwise = Gold (fromIntegral $ generateNumber (1,2)) $ Silver (fromIntegral $ generateNumber (1,900))

generateRandomSetItem :: Integer -> SetItem
generateRandomSetItem level = SetItem generateRandomClassSetItem (2 * (fromIntegral level)) "item" (fromIntegral level)

generateRandomWeapon :: Integer -> Weapon
generateRandomWeapon level = Weapon generateRandomClassWeapon "weapon" (12 * (fromIntegral level) ,20 * (fromIntegral level) ) (7 * (fromIntegral level)) 

generateRandomClassSetItem :: ClassItemSet
generateRandomClassSetItem = let numberClass = generateNumber (1,8)
                             in classSetItemByNumber numberClass

generateRandomClassWeapon :: ClassWeapon
generateRandomClassWeapon = let numberClass = generateNumber (1,12)
                            in classWeaponByNumber numberClass

classWeaponByNumber :: Integer -> ClassWeapon
classWeaponByNumber 1  =  Dagger
classWeaponByNumber 2  =  Blade
classWeaponByNumber 3  =  GreatSword
classWeaponByNumber 4  =  Hammer
classWeaponByNumber 5  =  Maze
classWeaponByNumber 6  =  Fists
classWeaponByNumber 7  =  Wand
classWeaponByNumber 8  =  Stick
classWeaponByNumber 9  =  Bow
classWeaponByNumber 10 =  CrossBow
classWeaponByNumber 11 =  Guns
classWeaponByNumber 12 =  Axe


classSetItemByNumber :: Integer -> ClassItemSet
classSetItemByNumber 1  =  Armor
classSetItemByNumber 2  =  Pant
classSetItemByNumber 3  =  Gloves
classSetItemByNumber 4  =  Shield
classSetItemByNumber 5  =  Boots
classSetItemByNumber 6  =  Helm
classSetItemByNumber 7  =  Pendant
classSetItemByNumber 8  =  Ring

