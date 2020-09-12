{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M

import ExprT
import Parser
import qualified StackVM




eval :: ExprT -> Integer
eval (ExprT.Lit i)  = i
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                  Just expr -> Just (eval expr)
                  Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul


reify :: ExprT -> ExprT
reify = id


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool  where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 x) (Mod7 y) = lit $ x+y
    mul (Mod7 x) (Mod7 y) = lit $ x*y

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"


--------------------------------------------------
instance Expr StackVM.Program where
    lit n = [StackVM.PushI n]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe  StackVM.Program
compile = parseExp lit add mul 

------------------------------------------------
class HasVars a where
    var :: String -> a

data VarExprT = Lit' Integer
              | Var String
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
  deriving (Show,Eq)

instance Expr VarExprT where
    lit n = Lit' n
    add x y = Add' x y 
    mul x y = Mul' x y

instance HasVars VarExprT where
    var s = Var s

f :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
f op (Just x) (Just y) = Just (op x y)
f _ _ _ = Nothing 

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add e1 e2 = \m -> f (+) (e1 m) (e2 m)
    mul e1 e2 = \m -> f (*) (e1 m) (e2 m)
    
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = \m -> M.lookup s m
  
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs