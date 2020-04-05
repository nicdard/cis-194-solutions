{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toLower)
import Data.List (foldl')

-- Exercise 3
newtype Score = Score Int
    deriving (Show, Ord, Num, Eq)

getScore :: Score -> Int
getScore (Score x) = x

class Scored a where
    score :: a -> Score

instance Scored Score where
    score = id

instance Scored a => Scored (a,b) where
    score = score . fst

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

scoreChar :: Char -> Score
scoreChar c
    | low `elem` "aeiounrstl"   = Score 1
    | low `elem` "dg"           = Score 2
    | low `elem` "bcpm"         = Score 3
    | low `elem` "fvwyh"        = Score 4
    | low == 'k'                = Score 5
    | low `elem` "xj"           = Score 8
    | low `elem` "qz"           = Score 10
    | otherwise                 = mempty
    where low = toLower c

scoreString :: String -> Score
scoreString = mconcat . map scoreChar


