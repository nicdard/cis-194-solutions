{-# OPTIONS_GHC -Wall #-}
module Party where

import Employee
import Data.Tree
import Data.List
import Data.Function

-- Exercise 1.1
-- adds an employee to the GL (updating the cached Fun score)
glCons :: Employee -> GuestList -> GuestList
glCons e@Emp {empFun = fun} (GL list totalFun) = GL (e:list) $ fun + totalFun

-- Exercise 1.2
instance Semigroup GuestList where
    (<>) (GL l1 f1) (GL l2 f2) = GL (l1 <> l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

-- Exercise 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case gl1 `compare` gl2 of
    GT -> gl1
    _  -> gl2


-- Exercise 2
example :: Tree Int
example =
    Node {rootLabel = 3, subForest = [
        Node {rootLabel = 0, subForest = []},
        Node {rootLabel = 1, subForest = [
            Node 12 [],
            Node 1 []
        ]}
    ]}

example2 :: Tree String
example2 =
    Node {rootLabel = "a", subForest = [
        Node {rootLabel = "ba", subForest = []},
        Node {rootLabel = "ci", subForest = [
            Node "nt" [],
            Node "po" []
        ]}
    ]}

example3 :: Tree Int
example3 =
    Node 3 [
        Node 0 [],
        Node 2 [
            Node 0 [],
            Node 0 []
        ],
        Node 1 [
            Node 2 [
                Node 0 [],
                Node 0 []
            ]
        ]
    ]

{-
treeFold :: b -> (b -> a -> b) -> Tree a -> b
treeFold acc f Node {rootLabel = r, subForest = []} = f acc r
treeFold acc f Node {rootLabel = r, subForest = s}  =
    f (foldr (\it step -> treeFold step f it) acc s) r
-}

treeFold2 :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold2 acc f Node {rootLabel = r, subForest = []} = f r [acc]
treeFold2 acc f Node {rootLabel = r, subForest = s}  =
    f r (map (treeFold2 acc f) s)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel e []          = (glCons e mempty, mempty)
nextLevel e subresults  = (withBoss, withoutBoss)
    where
        withBoss = glCons e (mconcat $ map snd subresults)
        withoutBoss = mconcat $ map (uncurry moreFun) subresults

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold2 mempty nextLevel

-- Exercise 5
sortGuestList :: GuestList -> GuestList
sortGuestList (GL l fun) = GL (sortBy (compare `on` empName) l) fun

main :: IO ()
main = do
    content <- readFile "company.txt"
    let tree = read content :: Tree Employee
        (GL list fun) = sortGuestList . maxFun $ tree
    putStrLn $ "Total fun: " ++ show fun
    mapM_ (putStrLn . empName) list
