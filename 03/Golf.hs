module Golf where

import Data.List

-- Exercise 1 Hopscotch

-- Computes a single instance of the result.
takeEveryN :: [a] -> Int -> [a]
takeEveryN l n = reverse ll
    where (r, ll) = foldl f (n, []) l
            where f (r, acc) x = if r == 0
                    then (n, x:acc)
                    else (r-1, acc)

skips :: [a] -> [[a]]
skips l = map (takeEveryN l) [0 .. length l - 1]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:tail)
    | x < y && z < y    = y : localMaxima (y:z:tail)
    | otherwise         = localMaxima (y:z:tail)
localMaxima _           = []

-- Exercise 3
baseline = "==========\n0123456789\n"

-- Draws a line of the histogram, put ' ' on each column
-- that is less than the threshold and an '*' otherwise.
line :: Ord a => [a] -> a -> String
line l threshold = map (\x -> if x >= threshold then '*' else ' ') l

-- Computes a list containing the number of occurrences
-- of each element in [0..9] in l, each at its index
count :: (Eq a, Num a, Enum a) => [a] -> [Int]
count l = map (\x -> length $ filter (==x) l) [0..9]

histogram :: [Integer] -> String
histogram l = unlines (map (line c) [max + 1, max .. 1]) ++ baseline
    where c = count l
          max = maximum c

