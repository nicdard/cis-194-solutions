{-# OPTIONS_GHC -Wall #-}

module Sol04 where

-- Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- Redefined fun1
fun1' :: [Integer] -> Integer
fun1' = foldr (\x s -> (x - 2) * s) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n    = n + fun2 (n`div` 2)
        | otherwise = fun2 (3 * n + 1)

-- Redefined fun2
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>= 2) . iterate next
    where next x = if even x
            then x `div` 2
            else 3 * x + 1

-- Exercise 2: Folding with trees
data Tree a = Leaf
            -- The integer stores the height of the tree at the node
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr build Leaf
    where   build el Leaf             = Node 0 Leaf el Leaf
            build el (Node h sx e dx) = case treeCompare sx dx of
                EQ -> Node h (build el sx) e dx
                LT -> Node (h+1) (build el sx) e dx
                GT -> Node (h+1) sx e (build el dx)
                where   treeCompare Leaf Leaf                       = EQ
                        treeCompare Leaf Node {}                    = LT
                        treeCompare Node {} Leaf                    = GT
                        treeCompare (Node hx _ _ _) (Node hd _ _ _) = compare hx hd

-- Exercise 3: More folds!
-- 1
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then 1 + acc else acc) 0

-- 2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

--3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4: Finding primes
{- Start with a list of the integers from 1 to n.
   From this list, remove all numbers of the form i + j + 2ij where:
    .  i, j in N, 1 <= i <= j
    .. i + j + 2ij <= n
   The remaining numbers are doubled and incremented by one,
   giving a list of the odd prime numbers
   (i.e., all primes except 2) below 2n + 2. -}

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let toExclude   = map (uncurry sundaramForm)
                    . filter (\(a, b) -> a <= b && sundaramForm a b <= n)
                    $ cartProd [1..n] [1..n]
            where   sundaramForm a b = a + b + 2 * a * b
    in  map ((+1) . (*2))
        . filter (`notElem` toExclude)
        $ [1..n]
