module Character (newCharacter, addExperience, changeWeapon, receiveAttack, generateRandomDmg, isDead) where

import qualified Data.Map as M 
import Data.Maybe
import System.Random
import System.IO
import System.IO.Unsafe
import Item

data Character = Character Character.Name Level Stats Weapon {--SetItems TreeWeapons --} deriving (Show, Eq)

-- quiero meter elementos (tal vez en el tree de las armas)

type Name = String

data Level = Level Int (Experience,ExperienceTotal) deriving (Show, Eq)

type Experience = Int

type ExperienceTotal = Int

data Stats = Stats (M.Map NameStat Int) Character.Damage Character.Defense Character.AttackSpeed Hp Energy deriving (Show, Eq)

data NameStat = Agility | Strength | Vitality | Intelligy deriving (Show, Eq, Ord)

type Damage = (Double,Double)

type Defense = Int

type AttackSpeed = Int

type Hp = (Int,Int)

type Energy = (Int,Int)

---------------------------------------------------
newCharacter :: Character.Name -> ClassWeapon -> Character
newCharacter name weapon = Character name basicLevel basicStats (Weapon weapon "Basic Weapon" (2,5) 5)

basicLevel :: Level
basicLevel = Level 1 (0,50)

basicStats :: Stats 
basicStats = Stats emptyStats (10,15) 10 2 (50,50) (50,50)

emptyStats :: M.Map NameStat Int
emptyStats = M.insert Intelligy 20 (M.insert Vitality 20 (M.insert Agility 20 (M.insert Strength 20 (M.empty))) )

addExperience :: Character -> Experience -> Character
addExperience (Character name level stats weapon) exp = let newlevel = (addExpToLevel level exp)
                                                        in Character name newlevel (calculateStats newlevel stats weapon) weapon

addExpToLevel :: Level -> Experience -> Level
addExpToLevel (Level l (expA,expM)) exp | (expA + exp) >= expM = addExpToLevel (Level (l+1) (0, (expM * 2)) ) (exp - (expM - expA)) 
                                        | otherwise           = Level l (expA + exp,expM) 

calculateStats :: Level -> Stats -> Weapon -> Stats
calculateStats (Level l _) (Stats map dmg def as hp ene) (Weapon _ _ dmgW attS) = let str = fromIntegral $ fromJust (M.lookup Strength map) 
                                                                                  in let agi = fromJust (M.lookup Agility map) 
                                                                                     in let vit = fromJust (M.lookup Vitality map) 
                                                                                        in let int = fromJust (M.lookup Intelligy map) 
                                                                                           in Stats map (str * 0.5 + (fromIntegral l) + fst dmgW, str * 0.7 + (fromIntegral l) + snd dmgW) (agi * 2 + l) (agi + attS) (vit * 5 + (l * 2),vit * 5 + (l * 2)) (int * 5 + (l * 2),int * 5 + (l * 2))


changeWeapon :: Character -> Weapon -> Character
changeWeapon (Character name level stats _) newWeapon = (Character name level (calculateStats level stats newWeapon) newWeapon)

receiveAttack :: Character -> Character.Damage -> Character
receiveAttack (Character name level stats weapon) damage = let dmg = fromIntegral (generateRandomDmg (round $ fst damage,round $ snd damage))
                                                           in  Character name level (affectHp stats dmg) weapon 

generateRandomDmg :: (Integer,Integer) -> Integer
generateRandomDmg dmg =  unsafePerformIO $ getStdRandom $ randomR $ dmg

affectHp :: Stats -> Double -> Stats
affectHp s@(Stats map dmg def as hp ene) damage 
                                             | def - (round damage) < 0 = Stats map dmg def as ((fst hp) - ((round damage) - def) ,snd hp) ene
                                             | otherwise                = s

attack :: Character -> Character -> Character
-- first character attack to second character and return the second character injured
attack (Character _ _ stats _) secondChar = receiveAttack secondChar (characterDmg stats)

characterDmg :: Stats -> (Double,Double)
characterDmg (Stats _ dmg _ _ _ _) = dmg

isDead :: Character -> Bool
isDead (Character name level stats weapon) = hpIsLessOrEqualsTo0 stats

hpIsLessOrEqualsTo0 :: Stats -> Bool
hpIsLessOrEqualsTo0 (Stats _ _ _ _ hp _) = fst hp <= 0

---------------------------------------------------

trimegisto = newCharacter "trimegisto" Dagger
