{-# OPTIONS_GHC -Wall #-}

module Sol01 where

-- Pipe operator
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

-- Exercise 1:
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
    | n < 0               = toDigitsRev $ abs n
    | n >= 0 && n < 10    = [n]
    | otherwise           = n `mod` 10 : toDigitsRev (n `div` 10)
toDigits n = reverse $ toDigitsRev n

-- Exercise 2:
isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1

hasOddLength :: Foldable t => t a -> Bool
hasOddLength l = isOdd (length l)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther (x:xs)
   | hasOddLength xs    = x*2 : doubleEveryOther xs
   | otherwise          = x : doubleEveryOther xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:xs)  = x + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate creditCard = toDigits creditCard |> doubleEveryOther |> sumDigits `mod` 10 == 0

-- Exercise 5
type Peg    =    String
type Move   =    (Peg, Peg)
--       disks
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _           = []
hanoi 1 start end _     = [(start, end)]
hanoi n start end temp  =
   let nMinusOne = subtract 1 n
   in hanoi nMinusOne start temp end ++
      hanoi 1 start end temp ++
      hanoi nMinusOne temp end start

-- Exercise 6
hanoin :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoin 0 _ _ _ _            = []
hanoin 1 start end _ _      = [(start, end)]
hanoin 2 start end t1 _     = [(start, t1), (start, end), (t1, end)]
hanoin n start end t1 t2 =
   let nMinusTwo = subtract 2 n
   in hanoin nMinusTwo start t1 t2 end ++
      hanoin 2 start end t1 t2 ++
      hanoin nMinusTwo t1 end start t2
