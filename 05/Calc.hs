{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Map as M
import GHC.Base (liftA2)

import ExprT
import Parser
import StackVM


-- Exercise 1
eval :: ExprT -> Integer
eval e = case e of
    (Lit n)             -> n
    (ExprT.Add e1 e2)   -> eval e1 + eval e2
    (ExprT.Mul e1 e2)   -> eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr e = parseExp Lit ExprT.Add ExprT.Mul e >>= Just . eval

-- Exercise 3
newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit                         = MinMax
    add (MinMax a) (MinMax b)   = MinMax $ max a b
    mul (MinMax a) (MinMax b)   = MinMax $ min a b

instance Expr Mod7 where
    lit                     = Mod7 . (`mod` 7)
    add (Mod7 a) (Mod7 b)   = Mod7 . (`mod` 7) $ a + b
    mul (Mod7 a) (Mod7 b)   = Mod7 . (`mod` 7) $ a * b

-- Exercise 5
instance Expr Program where
    lit n   = [PushI n]
    add a b = b ++ a ++ [StackVM.Add]
    mul a b = b ++ a ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul

evalStackVM :: String -> Either String StackVal
evalStackVM s = let c = compile s
    in case c of
        Nothing -> Left "Nothing"
        Just p  -> stackVM p

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT   = LitV Integer
                | AddV VarExprT VarExprT
                | MulV VarExprT VarExprT
                | Var String
        deriving (Show, Eq)

instance Expr VarExprT where
    lit     = LitV
    add     = AddV
    mul     = MulV

instance HasVars VarExprT where
    var     = Var

{- variables can be interpreted as functions from
   a mapping of variables to Integer values to (possibly)
   Integer values -}
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

{- The second instance says that these same functions can be
   interpreted as expressions (by passing along the mapping to
   subexpressions and combining results appropriately) -}
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n       = \_ -> Just n
    add e1 e2   = \mapping -> liftA2 (+) (e1 mapping) (e2 mapping)
    mul e1 e2   = \mapping -> liftA2 (*) (e1 mapping) (e2 mapping)

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

