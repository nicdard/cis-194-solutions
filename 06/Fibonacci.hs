{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show s = foldl (\a it -> a ++ ' ':show it) ""
        $ take 20 (streamToList s)

streamToList :: Stream a -> [a]
streamToList (Cons f s) = f : streamToList s

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat el = Cons el (streamRepeat el)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons el s) = Cons (f el) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed t seed = Cons seed (streamFromSeed t $ t seed)

-- Exercise 5
-- The infinite list of natural numbers
nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons el1 s1) (Cons el2 s2) = Cons el1 $ Cons el2 (interleaveStreams s1 s2)

fact2 :: Integer -> Integer
fact2 n = let factR acc x = if even x
                then factR (acc + 1) $ x `div` 2
                else acc
          in factR 0 n

-- ruler function
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0)
    $ streamMap fact2 (streamFromSeed (+ 2) 2)

-- Exercise 6

-- Stores the coefficients of the generating function x where
-- x = 0 + 1x + 0x^2 + 0x^3 ...
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger i                   = Cons i (streamRepeat 0)
    negate                          = streamMap negate
    (+) (Cons el1 s1) (Cons el2 s2) = Cons (el1 + el2) (s1 + s2)
    (*) (Cons a0 ta) s@(Cons b0 tb) = Cons (a0 * b0) (streamMap (*a0) tb + ta * s)

instance Fractional (Stream Integer) where
    (/) (Cons a0 ta) (Cons b0 tb) = q
        where q = Cons (a0 `div` b0) (streamMap (`div` b0) (ta - q * tb))

-- Computes fibonacci sequence by generators
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Exercise 7
{- This type represent a 2x2 Matrix:
    e1 e2
    e3 e4 -}
data SquareMatrix2 a = SquareMatrix2 a a a a deriving Show
instance Num a => Num (SquareMatrix2 a) where
    (*) (SquareMatrix2 a1 a2 a3 a4) (SquareMatrix2 b1 b2 b3 b4) =
        SquareMatrix2 (a1 * b1 + a2 * b3) (a1 * b2 + a2 * b4)
                      (a3 * b1 + a4 * b3) (a3 * b2 + a4 * b4)

fib4 :: Integer -> Integer
fib4 n = let SquareMatrix2 f _ _ _ = SquareMatrix2 1 1 1 0 ^ n
         in f
