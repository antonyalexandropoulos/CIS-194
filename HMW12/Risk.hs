{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


battle :: Battlefield -> Rand StdGen Battlefield
battle b = sequence (replicate m die) >>= \attdice ->
           sequence (replicate n die) >>= \defdice ->
           return (battleDet b attdice defdice)
           where m = attackers b
                 n = defenders b

battleDet :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battleDet b attdice defdice = Battlefield (attackers b - attDead) (defenders b - defDead)
            where attDead = length $ filter (\(x,y) -> x - y <=0) sorted
                  defDead = length attdice - attDead
                  sorted  = zip (reverse $ sort attdice) (reverse $ sort defdice)

invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= \x ->
    if (attackers x > 2 || defenders x > 0)
    then invade x 
    else return b 

successProb :: Battlefield -> Rand StdGen Double
successProb b = sequence (replicate 1000 $ invade b) >>= \bs ->
                return ((fromIntegral $ length $ filter (\bb -> defenders bb == 0) bs) / 1000.0)
