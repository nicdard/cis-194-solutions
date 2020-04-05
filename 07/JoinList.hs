{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
     deriving (Eq)

-- Let's have a nicer representation than the default one,
-- we want here to see the structure also.
instance (Show m, Show a) => Show (JoinList m a) where
    show Empty          = "Empty\n"
    show (Single m a)   = "Single " ++ show m ++ " " ++ show a ++ "\n"
    show (Append m l r) = "Append " ++ show m ++ "\n" ++ show l ++ show r

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

-- Exercise 2.1
(!!?) :: [a] -> Int -> Maybe a
[]     !!?      _    = Nothing
_      !!? i | i < 0 = Nothing
(x:_) !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' i jl = jlToList jl !!? i

-- Fast indexing
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty                      = Nothing
indexJ index (Single _ a)
    | index == 0                    = Just a
    | otherwise                     = Nothing
indexJ index (Append _ l r)
    | index >= leftSize              = indexJ (index - leftSize) r
    | otherwise                     = indexJ index l
        where leftSize = getSize . size . tag $ l

-- Exercise 2.2
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n s@(Single _ _)
    | n <= 0            = s
dropJ n allT@(Append m l r)
    | n < 0             = allT
    | n >= rootSize     = Empty
    | n > leftSize      = Empty +++ dropJ (n - leftSize) r
    | otherwise         = dropJ n l +++ r
    where leftSize = getSize . size . tag $ l
          rootSize = getSize . size $ m
dropJ _ _               = Empty

-- Exercise 2.3
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ n s@(Single _ _)
    | n > 0             = s
takeJ n allT@(Append m l r)
    | n >= rootSize     = allT
    | n > leftSize      = l +++ takeJ (n - leftSize) r
    | n == leftSize     = l
    | n < leftSize      = takeJ n l
    where rootSize = getSize . size $ m
          leftSize = getSize . size . tag $ l
takeJ _ _               = Empty

-- To test the solutions
testJoinList :: JoinList Size Char
testJoinList =
  Append (Size 4)
    (Append (Size 3)
      (Single (Size 1) 'Y')
      (Append (Size 2)
        (Single (Size 1) 'e')
        (Single (Size 1) 's')))
    (Single (Size 1) '!')

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine ""    = Empty
scoreLine s     = Single (scoreString s) s

-- Exercise 4

createSingleLineBuff :: String -> JoinList (Score, Size) String
createSingleLineBuff s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
    toString        = unlines . jlToList

    fromString ""   = Empty
    fromString s    = foldr1 (+++) (map createSingleLineBuff $ lines s)

    line            = indexJ

    replaceLine n l buff =
        takeJ (n - 1) buff
        +++ createSingleLineBuff l
        +++ dropJ (n + 1) buff

    numLines        = getSize . size . tag
    value           = getScore . score . tag