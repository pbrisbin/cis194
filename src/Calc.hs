{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser

import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = const $ Just n
    add e1 e2 = \m -> (+) <$> e1 m <*> e2 m
    mul e1 e2 = \m -> (*) <$> e1 m <*> e2 m

withVars
    :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs e = e $ M.fromList vs

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

    compute :: String -> Maybe e
    compute = parseExp lit add mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n = n > 0
    add = (||)
    mul = (&&)

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr MinMax where
    lit = MinMax
    add (MinMax n) (MinMax m) = MinMax $ max n m
    mul (MinMax n) (MinMax m) = MinMax $ min n m

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 n) (Mod7 m) = lit $ n + m
    mul (Mod7 n) (Mod7 m) = lit $ n * m

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . compute
