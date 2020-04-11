{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Ord

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- Exercise 2
maxAttackers :: Army -> Army
maxAttackers a
    | disposableUnits <= 0  = 0
    | disposableUnits > 3   = 3
    | otherwise             = disposableUnits
    where disposableUnits = a - 1

maxDefenders :: Army -> Army
maxDefenders = min 2

-- Creates a list of n ordered rolled dices
orderedDices :: Int -> Rand StdGen [DieValue]
orderedDices n = sortOn Data.Ord.Down <$> replicateM n die

rollDices :: Battlefield -> Rand StdGen [(DieValue, DieValue)]
rollDices b = zip
           <$> orderedDices (maxAttackers . attackers $ b)
           <*> orderedDices (maxDefenders . defenders $ b)

deaths :: [(DieValue, DieValue)] -> (Int, Int)
deaths = foldl' (\(d1, d2) (i1, i2) -> if i1 > i2
                  then (d1, d2 - 1)
                  else (d1 - 1, d2)
               ) (0, 0)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = deaths <$> rollDices b
           >>= \dd -> return
                $ Battlefield
                    (attackers b + fst dd)
                    (defenders b + snd dd)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b
    | attackers b <= 1 || defenders b == 0  = return b
    | otherwise                             = battle b >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = (/1000) . genericLength . filter (\(Battlefield a d) -> a > d)
                <$> replicateM 1000 (invade b)

main :: IO ()
main = do
    let battlefield = Battlefield { attackers = 12, defenders = 10 }
    print =<< evalRandIO (battle battlefield)
    print =<< evalRandIO (invade battlefield)
    print =<< evalRandIO (successProb battlefield)
